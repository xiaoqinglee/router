PREREQUISITES:
nodejs

Install:
npm install

HTTP: run the router's branch https://github.com/apollographql/router-private/tree/igni/http_planner against this repo.

run
`npm run web`

and you're set

CLI:

plan example:
(note i havent tested the other cli parts)

```
npm run cli plan 'schema
  @link(url: "https://specs.apollo.dev/link/v1.0")
  @link(url: "https://specs.apollo.dev/join/v0.2", for: EXECUTION)
  @link(url: "https://specs.apollo.dev/inaccessible/v0.2", for: SECURITY) {
  query: Query
  subscription: Subscription
}

enum join__Graph {
  PRODUCTS @join__graph(name: "products", url: "http://localhost:8080/graphql")
}

directive @inaccessible on FIELD_DEFINITION | OBJECT | INTERFACE | UNION | ARGUMENT_DEFINITION | SCALAR | ENUM | ENUM_VALUE | INPUT_OBJECT | INPUT_FIELD_DEFINITION

directive @join__field(
  graph: join__Graph!
  requires: join__FieldSet
  provides: join__FieldSet
  type: String
  external: Boolean
  override: String
  usedOverridden: Boolean
) repeatable on FIELD_DEFINITION | INPUT_FIELD_DEFINITION

directive @join__graph(name: String!, url: String!) on ENUM_VALUE

directive @join__implements(
  graph: join__Graph!
  interface: String!
) repeatable on OBJECT | INTERFACE

directive @join__type(
  graph: join__Graph!
  key: join__FieldSet
  extension: Boolean! = false
  resolvable: Boolean! = true
) repeatable on OBJECT | INTERFACE | UNION | ENUM | INPUT_OBJECT | SCALAR

directive @link(
  url: String
  as: String
  for: link__Purpose
  import: [link__Import]
) repeatable on SCHEMA

scalar join__FieldSet

scalar link__Import

enum link__Purpose {
  """
  `SECURITY` features provide metadata necessary to securely resolve fields.
  """
  SECURITY

  """
  `EXECUTION` features provide metadata necessary for operation execution.
  """
  EXECUTION
}

type Subscription @join__type(graph: PRODUCTS) {
  notifyProductPriceChange(productId: ID): ProductPriceHistory
}

"ProductPriceHistory is the response object a the price history, a child of product"
type ProductPriceHistory @join__type(graph: PRODUCTS) {
  id: ID!
  price: Int!
  startDate: String!
}

type Query @join__type(graph: PRODUCTS) {
  hello: String!
}' 'query MyQuery { hello }' 'MyQuery'
```

outputs

```
{"usageReporting":{"statsReportKey":"# MyQuery\nquery MyQuery{hello}","referencedFieldsByType":{"Query":{"fieldNames":["hello"],"isInterface":false}}},"data":{"queryPlan":{"kind":"QueryPlan","node":{"kind":"Fetch","serviceName":"products","variableUsages":[],"operation":"query MyQuery__products__0{hello}","operationKind":"query","operationName":"MyQuery__products__0"}},"formattedQueryPlan":"QueryPlan {\n  Fetch(service: \"products\") {\n    {\n      hello\n    }\n  },\n}"}}
```
