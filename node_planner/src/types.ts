import { QueryPlannerConfig } from "@apollo/query-planner";

export type OperationResult =
  | { Ok: any; Err?: undefined }
  | { Ok?: undefined; Err: any };

export interface QueryPlannerConfigExt extends QueryPlannerConfig {
  graphqlValidation?: boolean;
}
