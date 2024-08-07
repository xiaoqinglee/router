use std::rc::Rc;

use apollo_compiler::collections::IndexMap;
use itertools::Itertools;
use jaq_interpret::Ctx;
use jaq_interpret::Filter;
use jaq_interpret::FilterT;
use jaq_interpret::RcIter;
use jaq_interpret::Val;
use serde_json_bytes::Value;

use crate::sources::connect::TransformError;

pub(super) fn execute(
    json: &Value,
    vars: &IndexMap<String, Value>,
    filter: &Filter,
) -> (Option<Value>, Vec<TransformError>) {
    // TODO: stop needing to convert to/from serde_json_bytes
    let json = val_from_serde_json_bytes(json.clone());

    // TODO: Are these pre-computed or can we look up values lazily?
    let inputs = RcIter::new(core::iter::empty());
    let var_values = vars
        .values()
        .map(|value| val_from_serde_json_bytes(value.clone()))
        .collect_vec();
    let out = filter.run((Ctx::new(var_values, &inputs), json));
    let mut errs = Vec::new();
    let mut output = None;
    for res in out {
        match res {
            Err(err) => errs.push(TransformError::Jq(err)),
            Ok(val) => {
                // TODO: Instead of converting as-is, inject typename where needed and apply aliases
                output = Some(Value::from(serde_json::Value::from(val)));
            }
        }
    }
    (output, errs)
}

#[cfg(test)]
mod test_execute {
    use jaq_syn::Lexer;
    use jaq_syn::Parser;
    use serde_json_bytes::json;
    use serde_json_bytes::ByteString;

    use super::*;
    use crate::sources::connect::Selection;

    #[test]
    fn vars() {
        let jq = "{greeting: $args, data: $this.something.nested}";
        let json = json!("");
        let mut vars = IndexMap::with_hasher(Default::default());
        vars.insert("args".to_string(), Value::String(ByteString::from("hello")));
        vars.insert("this".to_string(), json!({"something": {"nested": "here"}}));

        let selection = match Selection::parse_jq(jq).unwrap() {
            Selection::Jq { compiled, .. } => compiled,
            _ => panic!("Expected Jq selection"),
        };
        let (res, errs) = execute(&json, &vars, &selection);

        assert_eq!(errs, Vec::new());
        assert_eq!(res, Some(json!({"greeting": "hello", "data": "here"})));
    }

    #[test]
    /// This is stolen from apollo-router/src/plugins/connectors/tests.rs::test_selection_set
    fn complex() {
        let jq = r#"# jq
[.[].commit | {
    commit: {
        name_from_path: .author.name,
        by: {name: .author.name,email: .author.email, owner: $args.owner,}
    }
}]"#;
        let json = json!([{
          "sha": "abcdef",
          "commit": {
            "author": {
              "name": "Foo Bar",
              "email": "noone@nowhere",
              "date": "2024-07-09T01:22:33Z"
            },
            "message": "commit message",
          },
        }]);
        let selection = Selection::parse(jq).unwrap();
        let Selection::Jq { compiled, .. } = selection else {
            panic!("Expected Jq selection")
        };
        let mut vars = IndexMap::with_hasher(Default::default());
        vars.insert("args".to_string(), json!({"owner": "blah"}));

        let (res, errs) = execute(&json, &vars, &compiled);

        assert_eq!(errs, Vec::new());
        assert_eq!(
            res,
            Some(json!([{
                "commit": {
                    "name_from_path": "Foo Bar",
                    "by": {
                        "name": "Foo Bar",
                        "email": "noone@nowhere",
                        "owner": "blah"
                    }
                }
            }]))
        );
    }
}

fn val_from_serde_json_bytes(value: Value) -> Val {
    match value {
        Value::Null => Val::Null,
        Value::Bool(inner) => Val::Bool(inner),
        Value::Number(number) => {
            if let Some(int) = number.as_i64() {
                Val::Int(int as isize)
            } else {
                Val::Float(number.as_f64().unwrap())
            }
        }
        Value::String(inner) => Val::Str(Rc::new(inner.as_str().to_string())),
        Value::Array(innner) => Val::Arr(Rc::new(
            innner.into_iter().map(val_from_serde_json_bytes).collect(),
        )),
        Value::Object(inner) => Val::Obj(Rc::new(
            inner
                .into_iter()
                .map(|(k, v)| {
                    (
                        Rc::new(k.as_str().to_string()),
                        val_from_serde_json_bytes(v),
                    )
                })
                .collect(),
        )),
    }
}
