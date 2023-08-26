import { GraphQLErrorExt } from "@apollo/core-schema/dist/error.js";
import { ASTNode, Source, SourceLocation, ExecutionResult } from "graphql";
import {
  BridgeQueryPlanner,
  ExecutionResultWithUsageReporting,
  QueryPlanResult,
} from "./plan.js";
import { QueryPlannerConfigExt } from "./types.js";

type WorkerResult =
  | PlanResult
  | ApiSchemaResult
  | ExecutionResult
  | Map<string, string>
  | String;
// Plan result
type PlanResult =
  | ExecutionResultWithUsageReporting<QueryPlanResult>
  | FatalError;
export type ApiSchemaResult = {
  schema: string;
};

type FatalError = {
  errors: (JsError | WorkerGraphQLError)[];
};

type JsError = {
  name: string;
  message: string;
  stack?: string;
  validationError?: boolean;
};

type CauseError = {
  message: string;
  locations?: ReadonlyArray<SourceLocation>;
  extensions: {
    [key: string]: unknown;
  };
};

type WorkerGraphQLError = {
  name: string;
  message: string;
  locations?: ReadonlyArray<SourceLocation>;
  path?: ReadonlyArray<string | number>;
  extensions: {
    [key: string]: unknown;
  };
  nodes?: ReadonlyArray<ASTNode> | ASTNode;
  source?: Source;
  positions?: ReadonlyArray<number>;
  originalError?: Error;
  causes?: CauseError[];
  validationError?: boolean;
};
const isGraphQLErrorExt = (error: any): error is GraphQLErrorExt<string> =>
  error.name === "GraphQLError" || error.name === "CheckFailed";

const intoSerializableError = (error: Error): JsError => {
  const {
    name,
    message,
    stack,
    validationError = false,
  } = error as Error & { validationError?: boolean };
  return {
    name,
    message,
    stack,
    validationError,
  };
};

const intoCauseError = (error: any): CauseError => {
  const { locations, message, extensions } = error;
  return {
    locations,
    message,
    extensions,
  };
};

const intoSerializableGraphQLErrorExt = (
  error: GraphQLErrorExt<string> & { validationError?: boolean }
): WorkerGraphQLError => {
  const { message, locations, path, extensions } = error.toJSON();
  const {
    nodes,
    source,
    positions,
    originalError,
    name,
    validationError = false,
  } = error;
  const causes = (error as any).causes;
  return {
    name,
    message,
    locations,
    path,
    extensions,
    nodes,
    source,
    positions,
    originalError:
      originalError === undefined
        ? originalError
        : intoSerializableError(originalError),
    causes: causes === undefined ? causes : causes.map(intoCauseError),
    validationError,
  };
};

export let planners = new Map<number, BridgeQueryPlanner>();

export const updateQueryPlanner = (
  schema: string,
  options: QueryPlannerConfigExt,
  schemaId: number
): WorkerResult => {
  try {
    planners.set(schemaId, new BridgeQueryPlanner(schema, options));
    // This will be interpreted as a correct Update
    return {
      data: {
        queryPlan: { kind: "QueryPlan", node: null },
        formattedQueryPlan: "QueryPlan {}",
      },
      usageReporting: {
        statsReportKey: "",
        referencedFieldsByType: {},
      },
    };
  } catch (err) {
    // The error that has been thrown needs to be sent back
    // to the rust runtime. In order to do so, it will be serialized.
    // The code below will allow us to build an object that is JSON serializable,
    // yet contains all of the information we need
    const errorArray = Array.isArray(err) ? err : [err];
    const errors = errorArray.map((err) => {
      if (isGraphQLErrorExt(err)) {
        return intoSerializableGraphQLErrorExt(err);
      } else {
        return intoSerializableError(err);
      }
    });

    return { errors };
  }
};
