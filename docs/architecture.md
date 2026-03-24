# Skyve Architecture

This document is a current high-level overview of how the Skyve Java framework is structured in this repository. It is intended to help maintainers and coding agents orient themselves quickly, understand the main ownership boundaries, and make changes at the right layer.

## What Skyve Is

Skyve is a metadata-driven Java low-code framework for building multi-tenant business applications. The framework combines:

- XML metadata for customers, modules, documents, views, routing, security, and behaviour
- Java extension points for actions, bizlets, services, jobs, reports, and integration logic
- Code generation for typed domain artefacts and supporting test scaffolding
- Web delivery layers and packaging for deployable applications

The practical implication is that Skyve behaviour is usually explained by a combination of handwritten Java, metadata, and generated output rather than by any one layer in isolation.

## Current Module Layout

The root Maven reactor in [pom.xml](../pom.xml) defines the current build graph.

| Module | Responsibility |
| --- | --- |
| `skyve-core` | Core contracts, metadata models, binding, types, repository abstractions, persistence/web interfaces, and foundational utilities. |
| `skyve-ext` | Concrete runtime services layered on core, including persistence support, jobs, reporting, backup, search, scripting, mail, caching, and other integrations. |
| `skyve-web` | Web-facing delivery layer including JSF/Faces pipelines, filters, servlet integration, web services, and browser-facing framework behaviour. |
| `skyve-war` | Assembled reference application and deployable WAR, including admin metadata, customer configuration, and generated domain/test sources. |
| `skyve-content` | Content extraction and indexing add-ins used by the framework. |
| `skyve-tools` | Supporting tools and assets, including SAIL-related tooling. |
| `skyve-maven-plugin` | Maven goals for metadata validation, domain generation, scaffolding, assembly, and documentation generation. |
| `skyve-jar` | Standalone packaging around the Skyve web stack. |

Other directories may exist for historical, experimental, or client-specific purposes, but the reactor modules above are the authoritative architectural backbone for this repository.

## Architectural Principles

- Metadata-first: customer/module/document/view/router metadata is a primary source of truth.
- Generated-not-handwritten: generated domain and test artefacts explain runtime shape, but they should normally be regenerated, not edited manually.
- Layered ownership: contracts belong in `skyve-core`, concrete services in `skyve-ext`, delivery concerns in `skyve-web`, and assembled runtime packaging in `skyve-war`.
- Multi-tenant by design: customer context, overrides, role-based security, and routing are framework-level concerns.
- Extensible by code and metadata: Skyve supports declarative behaviour first, with Java extension points where imperative logic is needed.

## How Behaviour Flows Through The System

Most non-trivial framework behaviour follows this path:

1. Metadata is loaded and validated.
2. The generator/plugin layer derives typed domain artefacts and supporting code from that metadata.
3. Core APIs expose the resulting model, binding, repository, and persistence contracts.
4. Runtime services in `skyve-ext` implement the heavier framework behaviour such as persistence, jobs, reporting, content, search, and integration concerns.
5. `skyve-web` interprets metadata and services into web requests, UI pipelines, filters, and web-service behaviour.
6. `skyve-war` assembles metadata, generated code, resources, and packaging into the runnable reference application.

When debugging or designing a change, identify which step is the real source of truth before editing code.

## Metadata And Generation

Metadata is central to Skyve. It defines structure and behaviour for:

- customers and tenant-specific overrides
- modules and document models
- views and routing
- roles and privileges
- actions, bizlets, and related behaviour hooks

The Maven plugin is responsible for validating that metadata and generating supporting artefacts. In this repository, `skyve-war` is the main assembled application module and contains configured generation inputs plus generated outputs under:

- `skyve-war/src/generated/java`
- `skyve-war/src/generatedTest/java`

If metadata changes but generation has not been rerun, compile and test results may be misleading.

## Runtime Layers

### `skyve-core`

`skyve-core` defines the framework vocabulary: metadata model, types, binding, persistence contracts, repositories, user/security abstractions, and core utility code. This is the layer that other modules depend on.

### `skyve-ext`

`skyve-ext` turns the core contracts into concrete runtime behaviour. This includes persistence implementations, reporting support, scheduling/jobs, mail, search/content support, scripting, backup, and other framework services.

### `skyve-web`

`skyve-web` is the delivery layer for browser and HTTP-facing concerns. It contains servlet integration, filters, Faces pipelines, web services, and related rendering or request-processing code.

### `skyve-war`

`skyve-war` is where the framework is assembled into a deployable application. It contains admin/customer metadata, generated domain outputs, test fixtures, and WAR packaging rules.

## Customisation Model

Skyve applications are usually customised through a mix of metadata and extension code:

- metadata declares structure and default behaviour
- generated domain code provides typed artefacts from that declaration
- handwritten extension classes provide imperative custom behaviour where needed
- customer-specific overrides refine behaviour without forking the framework model

That is why small fixes often require checking four places:

- the metadata
- the generator or plugin configuration
- the generated output
- the handwritten runtime extension

## Testing Model

The repository uses a mix of verification styles:

- Maven reactor builds from the root with `mvn -B package`
- Surefire unit tests run by default
- Failsafe integration tests are present but skipped by default unless enabled
- Some higher-cost UI/integration suites live under SAIL-oriented test packages and tooling
- Module-local tests span both JUnit 4 and JUnit 5

A good change workflow is:

1. Regenerate if metadata or generator inputs changed.
2. Run targeted module tests while iterating.
3. Run broader reactor validation before handing off.

## Practical Guidance For Maintainers

- Change the highest-level source of truth you can justify.
- Do not patch generated outputs when the generator input is wrong.
- Do not add delivery-specific concerns to `skyve-core`.
- Do not bypass framework abstractions just because a local fix looks smaller.
- When something looks unused, verify whether metadata, reflection, generation, or customer override paths still reference it.

## Pointers

- User-facing overview and setup guidance: [README.md](../README.md)
- Durable engineering guidance for agents: [docs/learnings.md](learnings.md)
- Plugin commands and generation tasks: [skyve-maven-plugin/readme.md](../skyve-maven-plugin/readme.md)
