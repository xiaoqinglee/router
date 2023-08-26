import express, { Express } from "express";
import http from "http";
import bodyParser from "body-parser";
import { updateQueryPlanner, planners, ApiSchemaResult } from "./index.js";

const app: Express = express();

app.use("*", bodyParser.json());

app.post("/schema", (req, res) => {
  const { sdl, config, schemaId } = req.body;
  const updateResult = updateQueryPlanner(sdl, config, schemaId);
  res.json(updateResult);
});

app.post("/schema/:schemaId/plan", (req, res) => {
  const { query, operationName } = req.body;

  const schemaId = req.params.schemaId;
  const planResult = planners.get(schemaId).plan(query, operationName);
  res.json(planResult);
});

app.get("/schema/:schemaId/apiSchema", (req, res) => {
  const apiSchemaResult = planners.get(req.params.schemaId).getApiSchema();

  const payload: ApiSchemaResult = { schema: apiSchemaResult };
  res.json(payload);
});

app.post("/schema/:schemaId/introspect", (req, res) => {
  const { query } = req.body;
  const schemaId = req.params.schemaId;

  const introspectResult = planners.get(schemaId).introspect(query);
  res.json(introspectResult);
});

app.post("/schema/:schemaId/signature", (req, res) => {
  const { query, operationName } = req.body;
  const schemaId = req.params.schemaId;
  const signature = planners
    .get(schemaId)
    .operationSignature(query, operationName);
  res.json(signature);
});

app.get("/schema/:schemaId/subgraphs", (req, res) => {
  const subgraphs = planners.get(req.params.schemaId).subgraphs();
  res.json(subgraphs);
});

const httpServer = http.createServer(app);

await new Promise<void>((resolve) =>
  httpServer.listen({ port: 4002 }, resolve)
);

console.log(`🚀 Planner available at http://localhost:4002/`);
