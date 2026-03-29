# Guidance for Project Agents

Skyve is a metadata-driven Java low-code framework implemented as a multi-module Maven repository. The source of truth is split between framework Java code, XML metadata, generated artefacts, and assembled runtime modules. Agent work must respect those boundaries.

## Required Reading

- [docs/learnings.md](docs/learnings.md) must always be read before starting work. It contains durable engineering guidance for this repository.
- [docs/architecture.md](docs/architecture.md) should be read whenever the task touches architecture, generated code, metadata flow, module boundaries, runtime bootstrapping, or delivery packaging.
- [README.md](README.md) and module-local docs should be treated as the source of truth for user-facing framework capabilities and supported workflows.

## Repository Shape

The current Maven reactor is defined by the root [pom.xml](pom.xml). The main modules are:

- `skyve-core` for public contracts, metadata models, type system, binding, repository abstractions, and core persistence/web contracts.
- `skyve-ext` for concrete runtime services layered on top of `skyve-core`, including persistence, reporting, backup, jobs, mail, scripting, search, and related integrations.
- `skyve-web` for the web delivery layer, JSF/Faces pipelines, filters, servlet integration, and web-facing services.
- `skyve-war` for the assembled reference application, admin metadata, generated domain/test sources, and deployable WAR packaging.
- `skyve-content` for content extraction and indexing add-ins.
- `skyve-maven-plugin` for metadata validation, domain generation, scaffolding, and system-documentation generation.
- `skyve-tools` for supporting build/test tooling, including SAIL-related assets.
- `skyve-jar` for standalone packaging around the web stack.

Other directories exist in the repository, but do not assume they participate in the default reactor build unless they are listed in the root `pom.xml`.

## Cross-Cutting Invariants

- Metadata is primary. If behaviour is declared in customer/module/document/view/router metadata, change the metadata first and regenerate before patching runtime Java around it.
- Generated sources are derived artefacts. Do not hand-edit `src/generated/java` or `src/generatedTest/java` unless the user explicitly asks for generated output surgery and the regeneration path is unavailable.
- Customer overrides and multi-tenancy are first-class design constraints. Avoid introducing hard-coded behaviour that bypasses customer-specific metadata, repository lookup, or security rules.
- Keep dependency direction intact. Prefer changes in the highest layer that owns the concern instead of leaking web/runtime concerns downward into `skyve-core`.
- Backward compatibility matters. Skyve exposes framework APIs, metadata formats, generated-code conventions, and runtime behaviour used by downstream applications.
- Security, routing, validation, and persistence rules are part of the framework contract. Avoid bypassing framework abstractions with ad hoc SQL, servlet logic, or direct UI assumptions when an existing Skyve service or metadata mechanism already owns the behaviour.

## Codebase Style

- Keep changes simple, explicit, and minimal. Reduce concept count before adding helpers, but do not duplicate logic when a clean shared abstraction already exists.
- Prefer documenting invariants and contracts over writing commentary about obvious mechanics.
- Name side-effecting methods so the mutation or external effect is obvious.
- When the framework already has a pattern for a concern, follow the pattern. In Skyve this often means changing metadata, generator inputs, extension points, or service abstractions instead of inserting special cases.
- Treat runtime services as managed collaborators. Prefer existing CDI/Spring injection patterns over constructing service objects ad hoc, and introduce an explicit test seam if a class must be instantiated directly in tests.
- Avoid introducing new dependencies or frameworks unless the existing stack is demonstrably insufficient.
- Preserve existing test style within the area you are touching. This repository currently contains both JUnit 4 and JUnit 5 tests.
- Do not delete seemingly-unused code until you understand whether it participates in generation, reflection, metadata lookup, CDI/Spring wiring, or customer override behaviour.

## Change Workflow

- Inspect the existing code, metadata, generated artefacts, and tests before designing a fix.
- If the task affects documents, modules, views, routes, or generated domain classes, identify the metadata source of truth and the generation step before editing anything.
- Prefer targeted Maven commands while iterating, then run broader validation when the change settles.
- If metadata has changed, regenerate before trusting compile or test results.
- If a prerequisite is wrong, fix the prerequisite cleanly instead of layering a workaround on top.

Useful commands:

- Repository build: `mvn -B package`
- Targeted module tests: `mvn -pl skyve-core -am test`
- Generated-domain refresh for the assembled app: run `mvn skyve:generateDomain` from `skyve-war/`
- Full verification including failsafe tests when relevant: `mvn -B verify -DskipIntegrationTests=false`
- Plugin help and targets: see [skyve-maven-plugin/readme.md](skyve-maven-plugin/readme.md)

## Testing Expectations

- If you changed Java code, metadata, generators, or packaging behaviour, run the narrowest meaningful validation first and widen from there.
- `mvn -B package` from the repository root is the baseline CI-aligned build.
- Surefire unit tests run by default; failsafe integration tests are skipped by default via the root Maven properties unless explicitly enabled.
- Some higher-cost test suites live under SAIL/integration-oriented packages. Run them when the touched behaviour warrants it, but do not claim they ran if you only executed unit/module tests.
- In H2-backed framework tests, prefer existing test bases such as `AbstractH2Test`, `AbstractDomainTest`, or `AbstractActionTest` when they fit the change.
- When creating real Skyve document instances, prefer the generated `DocumentName.newInstance()` path rather than calling constructors directly. In database-backed tests, prefer `DataBuilder` for valid fixture graphs when the surrounding code already follows that pattern.
- Documentation-only changes do not require a full reactor build, but they should still be reviewed for command accuracy and consistency with the current reactor.

## Debugging Pointers

Common places to look:

- Metadata loading and validation: `skyve-core` metadata/repository packages and `skyve-maven-plugin`
- Domain generation and scaffolding: `skyve-maven-plugin`, plus generated sources under `skyve-war/src/generated*`
- Runtime services and persistence behaviour: `skyve-ext`
- Web request handling, Faces pipelines, filters, and auth/web concerns: `skyve-web`
- Assembled admin/customer metadata and deployable runtime wiring: `skyve-war`

## Agent Peer Review

For design-heavy or architecture-sensitive changes, it is useful to get a second agent opinion before locking in a plan.

- Claude and Codex can be used via the human by preparing a prompt file and asking for the response to be written back to a file in this repository.
- Good prompts point the reviewing agent at a small set of files and ask for a written review of invariants, risks, and cleaner alternatives.

## Working With The Human

- Close the loop. If you make code changes, validate them before presenting the work as complete.
- Be explicit when a result depends on generated artefacts, environment setup, database state, or application-server configuration.
- Treat the human as a tool only for steps that genuinely require human action, such as credentials, external infrastructure access, or approvals outside the sandbox.
- When asking the human to do something, give the exact command or exact UI steps and explain what completion looks like.
