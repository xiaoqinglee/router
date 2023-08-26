import { updateQueryPlanner, planners, ApiSchemaResult } from "./index.js";
import * as fs from 'fs';

interface Plan {
  schema: string;
  query: string;
  operationName?: string;
}
interface ApiSchema {
  schema: string;
}

interface Introspect {
  schema: string;
  query: string;
}

interface Signature {
  schema: string;
  query: string;
  operationName?: string;
}

interface Subgraphs {
  schema: string;
}

const [command, ...query] = process.argv.slice(2);

const schema = fs.readFileSync(0, 'utf-8');

const rest:string[] = [schema, query[0]];

let result: unknown;
switch (command) {
  // subgraphs <schema>
  case "subgraphs":
    const subgraphs: Subgraphs = {
      schema: rest[0],
    };
    updateQueryPlanner(subgraphs.schema, { graphqlValidation: false }, 1);
    result = planners.get(1).subgraphs();
    console.log(JSON.stringify(result));
    break;
  // plan <schema> <query> <operationName>
  case "plan":
    const plan: Plan = {
      schema: rest[0],
      query: rest[1],
      operationName: rest[2],
    };
    updateQueryPlanner(plan.schema, { graphqlValidation: false }, 1);
    result = planners.get(1).plan(plan.query, plan.operationName);
    console.log(JSON.stringify(result));
    break;
  // apiSchema <schema>
  case "apiSchema":
    const apiSchema: ApiSchema = {
      schema: rest[0],
    };
    updateQueryPlanner(apiSchema.schema, { graphqlValidation: false }, 1);
    result = planners.get(1).getApiSchema();
    console.log(JSON.stringify(result));
    break;
  case "introspect":
    const introspect: Introspect = {
      schema: rest[0],
      query: rest[1],
    };
    updateQueryPlanner(introspect.schema, { graphqlValidation: false }, 1);
    result = planners.get(1).introspect(introspect.query);
    console.log(JSON.stringify(result));
    break;
  case "signature":
    const signature: Signature = {
      schema: rest[0],
      query: rest[1],
      operationName: rest[2],
    };
    updateQueryPlanner(signature.schema, { graphqlValidation: false }, 1);
    result = planners.get(1).operationSignature(signature.query, signature.operationName);
    console.log(JSON.stringify(result));
    break;
  default:
    console.log(JSON.stringify({ error: "no command provided" }));
    process.exit(1);
}
