use serde_json_bytes::Value as JSON;

use super::immutable::InputPath;
use super::location::WithRange;
use super::ApplyToError;
use super::MethodArgs;
use super::PathList;
use super::VarsWithPathsMap;

// Two kinds of methods: public ones and not-yet-public ones. The future ones
// have proposed implementations and tests, and some are even used within the
// tests of other methods, but are not yet exposed for use in connector schemas.
// Graduating to public status requires updated documentation, careful review,
// and team discussion to make sure the method is one we want to support
// long-term. Once we have a better story for checking method type signatures
// and versioning any behavioral changes, we should be able to expand/improve
// the list of public::* methods more quickly/confidently.
#[cfg(test)]
mod future;
mod public;

#[cfg(test)]
mod tests;

#[derive(Clone, Copy, Debug)]
pub(crate) enum ArrowMethod {
    /// This built-in method returns its first input argument as-is, ignoring
    /// the input data. Useful for embedding literal values, as in
    /// $->echo("give me this string").
    Echo,
    /// When invoked against an array, ->map evaluates its first argument
    /// against each element of the array and returns an array of the
    /// results. When invoked against a non-array, ->map evaluates its first
    /// argument against the data and returns the result.
    Map,
    /// Takes any number of pairs [candidate, value], and returns value for
    /// the first candidate that equals the input data $. If none of the
    /// pairs match, a runtime error is reported. The pattern [@, value] will always match.
    Match,
    /// Get the first element of an array
    First,
    /// Get the last element of an array
    Last,
    /// Get a subarray of the input array
    Slice,
    /// The size of an array or object, as a number
    Size,
    /// Returns a list of [{ key, value }, ...] objects for each key-value pair in
    /// the object. Returning a list of [[ key, value ], ...] pairs might also seem
    /// like an option, but GraphQL doesn't handle heterogeneous lists (or tuples) as
    /// well as it handles objects with named properties like { key, value }.
    Entries,
    #[cfg(test)]
    /// Returns the type of the data as a string, e.g. "object", "array",
    /// "string", "number", "boolean", or "null". Note that `typeof null` is
    /// "object" in JavaScript but "null" for our purposes.
    TypeOf,
    #[cfg(test)]
    /// Returns true if the data is deeply equal to the first argument, false
    /// otherwise. Equality is solely value-based (all JSON), no references.
    Eq,
    #[cfg(test)]
    /// Like ->match, but expects the first element of each pair to evaluate
    /// to a boolean, returning the second element of the first pair whose
    /// first element is true. This makes providing a final catch-all case
    /// easy, since the last pair can be [true, <default>].
    MatchIf,
    #[cfg(test)]
    Add,
    #[cfg(test)]
    Sub,
    #[cfg(test)]
    Mul,
    #[cfg(test)]
    Div,
    #[cfg(test)]
    Mod,
    #[cfg(test)]
    Has,
    #[cfg(test)]
    Get,
    #[cfg(test)]
    Keys,
    // Untested future variants
    // Values,
    // And,
    // Or,
    // Not,
}

impl ArrowMethod {
    pub(crate) fn execute(
        &self, // Method name
        method_name: &WithRange<String>,
        // Arguments passed to this method
        method_args: Option<&MethodArgs>,
        // The JSON input value (data)
        data: &JSON,
        // The variables
        vars: &VarsWithPathsMap,
        // The input_path (may contain integers)
        input_path: &InputPath<JSON>,
        // The rest of the PathList
        tail: &WithRange<PathList>,
    ) -> (Option<JSON>, Vec<ApplyToError>) {
        match self {
            ArrowMethod::Echo => {
                public::echo_method(method_name, method_args, data, vars, input_path, tail)
            }
            ArrowMethod::Map => {
                public::map_method(method_name, method_args, data, vars, input_path, tail)
            }
            ArrowMethod::Match => {
                public::match_method(method_name, method_args, data, vars, input_path, tail)
            }
            ArrowMethod::First => {
                public::first_method(method_name, method_args, data, vars, input_path, tail)
            }
            ArrowMethod::Last => {
                public::last_method(method_name, method_args, data, vars, input_path, tail)
            }
            ArrowMethod::Slice => {
                public::slice_method(method_name, method_args, data, vars, input_path, tail)
            }
            ArrowMethod::Size => {
                public::size_method(method_name, method_args, data, vars, input_path, tail)
            }
            ArrowMethod::Entries => {
                public::entries_method(method_name, method_args, data, vars, input_path, tail)
            }
            #[cfg(test)]
            ArrowMethod::TypeOf => {
                future::typeof_method(method_name, method_args, data, vars, input_path, tail)
            }
            #[cfg(test)]
            ArrowMethod::Eq => {
                future::eq_method(method_name, method_args, data, vars, input_path, tail)
            }
            #[cfg(test)]
            ArrowMethod::MatchIf => {
                future::match_if_method(method_name, method_args, data, vars, input_path, tail)
            }
            #[cfg(test)]
            ArrowMethod::Add => {
                future::add_method(method_name, method_args, data, vars, input_path, tail)
            }
            #[cfg(test)]
            ArrowMethod::Sub => {
                future::sub_method(method_name, method_args, data, vars, input_path, tail)
            }
            #[cfg(test)]
            ArrowMethod::Mul => {
                future::mul_method(method_name, method_args, data, vars, input_path, tail)
            }
            #[cfg(test)]
            ArrowMethod::Div => {
                future::div_method(method_name, method_args, data, vars, input_path, tail)
            }
            #[cfg(test)]
            ArrowMethod::Mod => {
                future::mod_method(method_name, method_args, data, vars, input_path, tail)
            }
            #[cfg(test)]
            ArrowMethod::Has => {
                future::has_method(method_name, method_args, data, vars, input_path, tail)
            }
            #[cfg(test)]
            ArrowMethod::Get => {
                future::get_method(method_name, method_args, data, vars, input_path, tail)
            }
            #[cfg(test)]
            ArrowMethod::Keys => {
                future::keys_method(method_name, method_args, data, vars, input_path, tail)
            } // Untested future variants
              // ArrowMethod::Values => {
              //     future::values_method(method_name, method_args, data, vars, input_path, tail)
              // }
              // ArrowMethod::And => {
              //     future::and_method(method_name, method_args, data, vars, input_path, tail)
              // }
              // ArrowMethod::Or => {
              //     future::or_method(method_name, method_args, data, vars, input_path, tail)
              // }
              // ArrowMethod::Not => {
              //     future::not_method(method_name, method_args, data, vars, input_path, tail)
              // }
        }
    }

    pub(crate) fn lookup(method_name: &str) -> Option<Self> {
        match method_name {
            "echo" => Some(ArrowMethod::Echo),
            "map" => Some(ArrowMethod::Map),
            "match" => Some(ArrowMethod::Match),
            "first" => Some(ArrowMethod::First),
            "last" => Some(ArrowMethod::Last),
            "slice" => Some(ArrowMethod::Slice),
            "size" => Some(ArrowMethod::Size),
            "entries" => Some(ArrowMethod::Entries),
            #[cfg(test)]
            "typeof" => Some(ArrowMethod::TypeOf),
            #[cfg(test)]
            "eq" => Some(ArrowMethod::Eq),
            #[cfg(test)]
            "matchIf" => Some(ArrowMethod::MatchIf),
            #[cfg(test)]
            "add" => Some(ArrowMethod::Add),
            #[cfg(test)]
            "sub" => Some(ArrowMethod::Sub),
            #[cfg(test)]
            "mul" => Some(ArrowMethod::Mul),
            #[cfg(test)]
            "div" => Some(ArrowMethod::Div),
            #[cfg(test)]
            "mod" => Some(ArrowMethod::Mod),
            #[cfg(test)]
            "has" => Some(ArrowMethod::Has),
            #[cfg(test)]
            "get" => Some(ArrowMethod::Get),
            #[cfg(test)]
            "keys" => Some(ArrowMethod::Keys),
            _ => None,
        }
    }
}
