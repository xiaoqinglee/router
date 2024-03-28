# Subgraph Batching Prototype
Version: Draft 0.2

These notes are designed to maximise the utility of evaluating the subgraph batching prototype both for the customers involved and for Apollo.

Please read these notes carefully before:
 - Using the prototype router 
 - Reporting results back to Apollo

The [subgraph batching design document](https://docs.google.com/document/d/1KlGgNCm1sQWc-tYs2oqHauMBIYdrtlpIELySZDAnISE/edit?usp=sharing) may be helpful in evaluating the prototype.

## Using the prototype

### Deployment

#### Accessing the prototype

You can choose to build from source or from one of our pre-built options.

##### Source

The prototype router is publicly available on the main router repo on branch `preview-1-subgraph-batching`. It can be accessed and built as normal.

##### Binary

If you want to use a pre-built router, you can access the prototype assets as follows:
| Artifact |
| -------- |
| [macOS x86](https://output.circle-artifacts.com/output/job/04b4ee53-4216-466f-84b8-e45f12e14a46/artifacts/0/artifacts/router-v0.0.0-nightly.20240328+617c0745-x86_64-apple-darwin.tar.gz)
| [macOS Arm](https://output.circle-artifacts.com/output/job/7ad3359c-9645-47f8-80d1-edc60e383f43/artifacts/0/artifacts/router-v0.0.0-nightly.20240328+617c0745-aarch64-apple-darwin.tar.gz)
| [Windows](https://output.circle-artifacts.com/output/job/59d87a19-60f4-4c4d-b2bb-b6dcab9203d6/artifacts/0/artifacts/router-v0.0.0-nightly.20240328+617c0745-x86_64-pc-windows-msvc.tar.gz)
| [Linux x86](https://output.circle-artifacts.com/output/job/94cb9411-070b-4e6e-aef2-c74bcb859fa2/artifacts/0/artifacts/router-v0.0.0-nightly.20240328+617c0745-x86_64-unknown-linux-gnu.tar.gz)
| [Linux Arm](https://output.circle-artifacts.com/output/job/b8f0de9b-8db7-4395-a10a-ea0f225c6fc1/artifacts/0/artifacts/router-v0.0.0-nightly.20240328+617c0745-aarch64-unknown-linux-gnu.tar.gz)
| [Docker image](https://github.com/apollographql/router/pkgs/container/nightly%2Frouter/196919224?tag=v0.0.0-nightly.20240328-617c0745)
| [Docker Debug image](https://github.com/apollographql/router/pkgs/container/nightly%2Frouter/196919100?tag=v0.0.0-nightly.20240328-617c0745-debug)
| [Helm chart](https://github.com/apollographql/router/pkgs/container/helm-charts-nightly%2Frouter/196919393?tag=0.0.0-nightly.20240328-617c0745)


> Note: These assets are only available until 4/27/2024.

### Configuration

For testing this prototype these configuration notes should suffice.

The prototype is based off the latest version of the router so non subgraph batching specific documentation is [as usual](https://www.apollographql.com/docs/router/)

Subgraph batching may be enabled for either all subgraphs or named subgraphs as follows.

#### All Subgraphs

This snippet enables client-side and subgraph batching for all subgraphs.

```yaml
batching:
  enabled: true
  mode: batch_http_link
  subgraph:
    all:
      enabled: true
```
> Note: both `batching` and `batching.subgraph.all` must be enabled. `batching` enables client-side batching and everything under the optional `subgraph` key configures subgraph batching.

#### Named Subgraphs

This snippet enables client-side and subgraph batching for only the name `accounts` subgraph.

```yaml
batching:
  enabled: true
  mode: batch_http_link
  subgraph:
    subgraphs:
      accounts:
        enabled: true
```
> Note: `batching` must be enabled and `subgraphs` is a list of named subgraphs which must be enabled. `batching` enables client-side batching and everything under the optional `subgraph` key configures subgraph batching. If a subgraph is not named (i.e., not present in the subgraphs list), then batching is not enabled for that subgraph. A subgraph may also be present, but if `enabled` is `false`, that will also disable batching for that subgraph.

### Operation

During operation the prototype should behave like the latest release of the router. The following subgraph batching specific items will be of interest.

#### Metrics

The existing batching metrics, which are currently client-side batching specific, are enhanced with an optional `subgraph` attribute. If this attribute is not present, then the metrics relate to client side batching. If it is present, then the attribute will be the name of a subgraph and the metrics will relate to the named subgraph.

#### Tracing/Logging

#### Subgraph Request Span

This span has an attribute named: `graphql.operation.name`. Usually, for non-batched operations, this will contain the operation name for the request. However, for a batch of operations, there isn't a single operation name, for multiple operations are being made in the same request. We have taken the decision to set the `graphql.operation.name` attribute to "batch". Let us know if you have alternative suggestions or would like to see this treated a different way.

#### Additional Logging

Because this is prototype code, you'll see various logging statement at INFO level about the operation of the batching code. We'll either remove or lower the level of these log statements in the final release, so don't be concerned about these additional logs.

The additional logs will be useful when reporting issues to us.

#### Batch Tracing Enhancements

We've added support for [opentelemetry span linking](https://opentelemetry.io/docs/concepts/signals/traces/#span-links) so that we can indicate how individual request elements from a batch relate to the overall batch fetches.

We've included a couple of jaeger screen shots (in the root directory of the prototype branch) to illustrate how this looks if you are making the sample queries in our prototype documentation.

Example1.png
Example2.png

In the screenshots you can see that jaeger is indicating the linked relationship of the spans with the "umbrella" and "up-arrow" icons on the right of the respective lines in the "Service & Operation" columns.

Example 1 illustrates a more complex example where even batching federated requests (which require fetch sequencing) we can still achieve batching improvements.

Example 2 illustrates that for simple queries we can achieve large batching improvements. Note how every "subgraph_request" is identified as part of the "umbrella" "batch_request" and only one "http_request" is performed.

## Reporting Results

There are a number of things we are interested in receiving feedback on. We are categorising these issues as major/minor within the context of the prototype. Of course, all issues are important, but some will require more engineering re-working (major) whereas others are more likely to be easy to address before the project completes (minor). Anyway, the advice about classification is just that, so feel free to report an issue as major or minor. :)

Please only report issues in the prototype back to your account team in this format. Don't use GitHub to file issues as we'll find it harder to track and address them in the prototype.

Once you have finished evaluating the prototype, please let us know the following:

### Major Issues

Any major issues: router hangs, panics, missing or incorrect data, performance degradation, ...

### Minor Issues

Any minor issues: increased resource consumption, inaccurate metrics, confusing configuration, missing tracing details, ...

Please provide as much detail as possible for each issue you report. Please include whether you tested a router built from source or one of our pre-built binaries.

Feel free to provide multiple reports, but take some time to evaluate the prototype before reporting your first report. This will reduce overhead for all concerned.

# Sample Report

Customer: Starstuff

Contacts: engineer@starstuff.com, devops@starstuff.com

Evaluated: macOS Arm

Batching Configuration Snippet:
```yaml
batching:
  enabled: true
  mode: batch_http_link
  subgraph:
    all:
      enabled: true
```
Studio Graph/Variant Name (if applicable): starstuff@prod

Major Issues

1. When executing `ListCustomers` operation the router hangs without returning to the client.
2. During execution of `ListOrders` the router panics with the following error message: `panicked during batch assembly`.
3. How can I confirm that subgraph batching is working as designed?
4. The performance of non-batch operations is impacted with this prototype.

Minor Issues

1. The router is now consuming more memory with subgraph batching enabled. Previously: 150Mb, Now: 175Mb.
2. I can't tell from tracing records what the impact of subgraph batching is on traffic to my `accounts` subgraph.
