# Layer Inventory
This document is an investigation and overview of our tower layers, and layer-like things, before 2.0. It describes the order and the purpose of each layer, and whether it is Clone (which is roughly a stand-in for being "backpressured pipeline-ready").

This is ordered from the point of view of a request to the router, starting at the outer boundary and going "deeper" into the layer onion.

Still missing are the execution and subgraph client parts of the onion, and layers added by plugins.

## Axum
Before entering the router service, we have some layers on our axum Router. These are already functioning properly in the tower service stack.

- "Metrics handler"
  - It only implements the `apollo.router.operations` metric.
  - This is using `axum::middleware::from_fn`, so it is functioning properly as a tower layer.
- TraceLayer
  - This one is from `tower-http`!
- CorsLayer
  - This one is from `tower-http`!
- "License handler"
  - Logs a warning if the commercial licence is expired
  - Rejects requests if the commercial licence is "halted" (expired + a grace period)
  - This is using `axum::middleware::from_fn`, so it is functioning properly as a tower layer.
- RequestDecompressionLayer
  - This one is from `tower-http`!

Now, we enter `handle_graphql`.

- Compression
  - This is manually written inside `handle_graphql`, but could conceptually be considered a layer.
  - I don't see an obvious reason for why this could not use a standard tower-http compression layer.
- Then we create a router service and oneshot it.

## Router service
The router service consists of some layers in "front" of the service "proper", and of several layers *inside the router service*, which we appear to call manually.

I suspect that this is bad and that we should try to make all these layers part of a straightforward tower service stack.

Front (`RouterCreator`):
- StaticPageLayer
  - If configured, responds to any request that accepts a "text/html" response (*at all*, regardless of preference), with a fixed HTML response.
  - It is Clone!
  - This must occur before content negotiation, which rejects "Accept: text/html" requests.
- Content negotiation: Request-side
  - It is Clone!
  - This layer rejects requests with invalid Accept or Content-Type headers.

Plugins:
- Telemetry: InstrumentLayer
  - Only used with `SpanMode::Deprecated`
  - Maybe a candidate for removal in 2.0?
  - **It is not Clone**, but could be trivially derived, if we stick the span_fn in an Arc.
- Telemetry: other work
  - A lot of stuff is happening inside a `map_future` layer. I haven't checked this out but I think it's fine from a backpressure/pipeline perspective.
  - This would be easier to understand in a named layer, potentially.
- Body limiting (limits plugin)
  - Rejects bodies that are too big. It's an equivalent of the tower-http `RequestBodyLimitLayer`.
  - **It is not Clone**. We can trivially derive it, but, we should probably use tower-http's implementation.
  - There's a bit of a dance happening here that we can hopefully remove.
  - Comment: "This layer differs from the tower version in that it will always generate an error eagerly rather than allowing the downstream service to catch and handle the error."
  - I do not understand what that means. But it must be related to making the `map_error_to_graphql` call inside the limits plugin work.
  - It may not be trivial to change this to use tower-http.
- Traffic shaping:
  - Load shedding.
    - This is just tower!
  - TimeoutLayer
    - Only present if a timeout is configured in the router.
    - We use a `.map_result()` layer to handle the error and turn it into a GraphQL error response.
    - This is provided by `tower`!
  - Load shedding.
    - Do we need this twice? Wouldn't it all propagate to the outermost^ load shedding layer?
  - ConcurrencyLimitLayer
    - Only present if a concurrency limit is configured in the router.
    - We use a `.map_result()` layer to handle the error and turn it into a GraphQL error response.
    - This is provided by `tower`!
  - Load shedding.
    - Do we need this thrice? Wouldn't it all propagate to the outermost^ load shedding layer?
  - RateLimitLayer
    - Only present if a global rate limit is configured in the router.
    - We use a `.map_result()` layer to handle the error and turn it into a GraphQL error response.
    - This is provided by `tower`!
- Fleet detection
  - Records router request and response size (unless disabled by environment variable).
  - The size counting has the effect of turning all bodies into streams, which may have negative effects!
  - It is Clone.
- Authentication: InstrumentLayer
  - The same underlying implementation as the one in "Telemetry: InstrumentLayer", but creating a different span.
  - **It is not Clone**, but could be trivially derived, if we stick the `span_fn` in an Arc.
- Authentication: implementation
  - It reads a JWT from the request and adds various context values.
  - It can short-circuit on invalid JWTs.
  - This is just a checkpoint layer, so it will be easy to adapt
- File uploads
  - Processes multipart requests and enforces file uploads-specific limits (eg. max amount of files).
  - The multipart state is moved into extensions and the request is modified to look like a normal GraphQL request.
  - **It is not Clone**. This is a oneshot checkpoint layer. At first glance it does not look terribly difficult to change it to a normal checkpoint layer.
- Progressive override
  - Adds override labels used in the schema to the context. Coprocessors can supposedly use this.
  - It is Clone! This is using `map_request` from tower, so it's fine for backpressure.
- Rhai/coprocessors
  - I have not looked deeply into it but I think this will be okay

Proper (`RouterService`):
- Batching
  - This is not a layer but the code can sort of be understood conceptually like one. Maybe it could, should be a layer?
  - Splits apart the incoming request into multiple requests that go through the rest of the pipeline, and puts the responses back together.
- Persisted queries: Expansion
  - This expands requests that use persisted query IDs, and rejects freeform GraphQL requests if not allowed per router configuration.
  - This is *not* a real layer right now. I suspect it should be.
  - **It is not Clone**, but it looks easy to make it Clone: we can just derive it on this layer and on the ManifestPoller type it contains (which already uses Arc internally)
- APQs
  - Clients can submit a hash+query body to add a query to the cache. Afterward, clients can use only the hash, and this layer will populate the query string in the request body before query analysis.
  - This is *not* a real layer right now. I suspect it should be.
  - **It is not Clone**. I think it's nothing that a little Arc can't solve, but I didn't trace it deeply enough to be sure.
- Query analysis
  - This does query parsing and validation and schema-aware hashing,
    and field/enum usage tracking for apollo studio.
  - This is *not* a real layer right now. I suspect it should be.
  - It is Clone!
  - This includes an explicit call to the AuthorizationPlugin. I suspect the AuthorizationPlugin should instead add its own layer for this, but chances are there was a good reason to do it this way, like not having precise enough hook-points. Still, maybe it can be a separate layer that we add manually.
  - Query analysis also exposes a method that is used by the query planner service, which could just as well be a standalone function in my opinion.
- Persisted queries: Safelisting
  - This is *not* a real layer right now. I suspect it should be.
  - For requests *not* using persisted queries, this layer checks incoming GraphQL documents against the "free-form GraphQL behaviour" configuration (essentially: safelisting), and rejects requests that are not allowed.
  - For Clone-ness, see "Persisted queries: Expansion"
- Straggler bits, not part of sub-layers. I think some of these should be normal layers, and some of them should be just a `.map_response()` layer in the service stack.
  - It does something with the `Vary` header.
  - It adjusts the status code to 499 if the request was canceled.
  - It does various JSON serialization bits.
  - It does various telemetry bits such as counting errors.
  - It appears to do the *exact same thing* as "Content negotiation: Response-side" to populate the Content-Type header.

## Supergraph service
The supergraph service consists of some layers in "front" of the service "proper", and several interacting services *inside* the supergraph service.

The implementation of those interactions is more complicated than in the router service, but I think many things could probably be implemented as a normal tower service stack, and we could benefit from doing that.

Front (`SupergraphCreator`):
- Content negotiation: Response-side
  - It is Clone!
  - This layer sets the Content-Type header on the response.
- AllowOnlyHttpPostMutationsLayer is the final step before going into the supergraph service proper.
  - **It is not Clone** today but it can trivially be derived.

Plugin layers happen here or in between somewhere, TBD.
Plugins:
- CSRF
  - This is a checkpoint layer.
  - **It is not Clone**, but CheckpointLayer can trivially be made Clone, I think.
  - Rejects requests that might be cross-site request forgery...
  - TODO(@goto-bus-stop): I'm not actually sure how this works
- Telemetry: InstrumentLayer
  - The same underlying implementation as the one in `router_service`, but creating a different span.
  - **It is not Clone**, but could be trivially derived, if we stick the `span_fn` in an Arc.
- Telemetry: other work
  - A lot of stuff is happening inside a `map_response` and a `map_future` layer.
  - This copies several resources when the service is created, and uses them for the entire lifetime of the service. This will not do if we do not create the pipeline from scratch for every request.
  - This would be easier to understand if split apart into several named layers, potentially.
- Authorization
  - Rejects requests if not authenticated.
  - I'm extremely confused why this happens here as opposed to the authentication plugin.
  - This is a checkpoint layer.
- File uploads
  - Patches up variables in the parsed JSON to pass validation.
  - **It is not Clone**. This is a oneshot checkpoint layer, but it looks easy enough to update.
- Entity caching: Cache control headers
  - Sets cache control headers on responses based on context. The context is populated by the subgraph service.
- Progressive override
  - Collect overriden labels from the context (added by rhai or coprocessors), calculate the special percentage-chance labels to override, and add *all* enabled override labels to the context for use by the query planner service.
  - It is Clone! This is using `map_request` from tower, so it's fine for backpressure.
- Connectors
  - Not sure what this does in detail but it's using `map_future_with_request_data` which is probably fine.
- Rhai/coprocessors
  - I have not looked deeply into it but I think this will be okay

The bulk of the "Proper" `SupergraphService` is doing query planning, and then handing things off to the execution service. This probably could be conceptualised as 2 layers, or 3 if there is also one to handle subscriptions.
