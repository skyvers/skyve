# Project Agent Guide

Skyve is a metadata-driven Java low-code framework implemented as a multi-module Maven repository. The source of truth is split between framework Java code, XML metadata, generated artefacts, and assembled runtime modules. Agent work must respect those boundaries.

## Required Reading

- [learnings.md](learnings.md) must always be read before starting work. It contains durable engineering guidance for this repository.
- [plan.md](plan.md) must be read when asked to plan or execute a milestone or sprint. It defines the planning workflow, milestone plan format, execution steps, and the current roadmap.
- [test-patterns.md](test-patterns.md) should be read before writing or modifying tests.
- [../coverage-plan.md](../coverage-plan.md) must be read before starting any coverage-improvement work. It defines the target, skip list, and ranked package priorities.
- [../architecture.md](../architecture.md) should be read whenever the task touches architecture, generated code, metadata flow, module boundaries, runtime bootstrapping, or delivery packaging.
- [../../README.md](../../README.md) and module-local docs should be treated as the source of truth for user-facing framework capabilities and supported workflows.

## Repository Shape

The current Maven reactor is defined by the root [pom.xml](../../pom.xml). The main modules are:

- `skyve-core` for public contracts, metadata models, type system, binding, repository abstractions, and core persistence/web contracts.
- `skyve-ext` for concrete runtime services layered on top of `skyve-core`, including persistence, reporting, backup, jobs, mail, scripting, search, and related integrations.
- `skyve-web` for the web delivery layer, JSF/Faces pipelines, filters, servlet integration, and web-facing services.
- `skyve-war` for the assembled reference application, admin metadata, generated domain/test sources, and deployable WAR packaging.
- `skyve-content` for content extraction and indexing add-ins.
- `skyve-maven-plugin` for metadata validation, domain generation, scaffolding, and system-documentation generation.
- `skyve-tools` for supporting build/test tooling, including SAIL-related assets.
- `skyve-jar` for standalone packaging around the web stack.

Other directories exist in the repository, but do not assume they participate in the default reactor build unless they are listed in the root `pom.xml`.

## Ownership Map

Use this as the default placement guide for changes.

| Change Type | Primary Module | Typical Test Location | Notes |
|---|---|---|---|
| Public contracts, metadata model, core abstractions | `skyve-core` | `skyve-core/src/test/java` | Keep runtime integrations out of core |
| Persistence, reporting, backup, jobs, integrations | `skyve-ext` | `skyve-ext/src/test/java` or `skyve-war` H2 tests | Prefer integration seams already used in module |
| Web filters, Faces pipelines, servlet-facing code | `skyve-web` | `skyve-web/src/test/java` | Keep servlet concerns out of core |
| Assembled app metadata/admin behavior | `skyve-war` | `skyve-war/src/test/java` | Includes generated domain/test sources |
| Metadata validation or generation tooling | `skyve-maven-plugin` | `skyve-maven-plugin/src/test/java` | Validate generation side effects carefully |
| Coverage orchestration/report aggregation | `skyve-coverage` | Profile-driven build verification | Use `-Pcoverage` workflow |

## Definition of Done

All applicable rows must be satisfied before handoff.

| Area | Done When |
|---|---|
| Code | Behavior is implemented with minimal, scoped changes |
| Tests | Relevant tests are added/updated and executed at the right scope |
| Documentation | User/developer-facing docs and links are updated if behavior or paths changed |
| Validation Evidence | Commands run and outcomes are recorded accurately |
| Warnings/Diagnostics | Touched Java files are free of unresolved diagnostics |
| Compatibility | Backward-compatibility impact is stated (or explicitly none) |

## Cross-Cutting Invariants

- Metadata is primary. If behaviour is declared in customer/module/document/view/router metadata, change the metadata first and regenerate before patching runtime Java around it.
- Generated sources are derived artefacts. Do not hand-edit `src/generated/java` or `src/generatedTest/java` unless the user explicitly asks for generated output surgery and the regeneration path is unavailable.
- Customer overrides and multi-tenancy are first-class design constraints. Avoid introducing hard-coded behaviour that bypasses customer-specific metadata, repository lookup, or security rules.
- Keep dependency direction intact. Prefer changes in the highest layer that owns the concern instead of leaking web/runtime concerns downward into `skyve-core`.
- Backward compatibility matters. Skyve exposes framework APIs, metadata formats, generated-code conventions, and runtime behaviour used by downstream applications.
- Security, routing, validation, and persistence rules are part of the framework contract. Avoid bypassing framework abstractions with ad hoc SQL, servlet logic, or direct UI assumptions when an existing Skyve service or metadata mechanism already owns the behaviour.

### Security and Access Control Chain

- Implement access control as a chain of responsibility, not a single check. User attributes should be loaded at authentication, propagated through session/context, and enforced consistently at repository, router, and view layers.
- Role privileges for custom actions must be explicit. CRUD privilege alone does not imply action visibility or executability.
- Validate access on direct navigation as well as menu-driven navigation to prevent privilege escalation.

### Metadata-Driven Import and Relink Patterns

- Design import workflows to be metadata-driven: define file, record, field, and rule structure in metadata and drive parsing, validation, and persistence from those definitions.
- For replace/relink flows (for example graph re-import with dependencies), relink dependent records before deleting old graphs.
- Fail fast if incoming data omits claims still referenced by dependents.
- When introducing new dependent types, follow a consistent extension pattern: add service methods for detection and update, inject into the orchestration job, and add targeted tests for both no-dependents and failure paths.

## Codebase Style

- Keep changes simple, explicit, and minimal. Reduce concept count before adding helpers, but do not duplicate logic when a clean shared abstraction already exists.
- Prefer documenting invariants and contracts over writing commentary about obvious mechanics. Follow [javadoc-standards.md](javadoc-standards.md).
- Name side-effecting methods so the mutation or external effect is obvious.
- When the framework already has a pattern for a concern, follow the pattern. In Skyve this often means changing metadata, generator inputs, extension points, or service abstractions instead of inserting special cases.
- Avoid introducing new dependencies or frameworks unless the existing stack is demonstrably insufficient.
- Preserve existing test style within the area you are touching. This repository currently contains both JUnit 4 and JUnit 5 tests.
- Do not delete seemingly-unused code until you understand whether it participates in generation, reflection, metadata lookup, CDI/Spring wiring, or customer override behaviour.

## Formatting and Whitespace

- Formatting is part of the contract. Never submit generated or handwritten code with inconsistent indentation.
- For Java, match the surrounding file style exactly. In this repository, leading indentation in Java source is tab-based; do not replace leading tabs with spaces.
- Never mix leading tabs and spaces on the same indentation level.
- Before finalising Java edits, run a formatter check where available and fix all formatting drift.
- For `skyve-war`, use Spotless from the module directory:

```bash
mvn -Pspotless-with-download -DskipTests spotless:apply
```

- If touching files outside `skyve-war` where Spotless is not configured, still normalise indentation to match neighbouring code before completing the task.
- If the task produced poor indentation, fix whitespace first, then continue with behavioural changes.

## Change Workflow

- Inspect the existing code, metadata, generated artefacts, and tests before designing a fix.
- If the task affects documents, modules, views, routes, or generated domain classes, identify the metadata source of truth and the generation step before editing anything.
- Prefer targeted Maven commands while iterating, then run broader validation when the change settles.
- If metadata has changed, regenerate before trusting compile or test results.
- If a prerequisite is wrong, fix the prerequisite cleanly instead of layering a workaround on top.
- After editing any Java file, call `get_errors` on that file and resolve every warning before declaring the work complete. The project uses committed `.settings/org.eclipse.jdt.core.prefs` files that configure Eclipse JDT warning levels; VS Code surfaces these through `get_errors`. Common categories to watch: method can be declared static, deprecated API, autoboxing, potential resource leak, unnecessary `@SuppressWarnings`, raw types, and unused imports.
- When suppressing a warning, use the narrowest possible scope: prefer a declaration-level annotation (for example on a local variable or catch parameter) first, then method level, then class level only when the warning genuinely applies to the majority of members. Never suppress at a broader scope than necessary.
- In production code (non-test), `@SuppressWarnings` is a last resort. Fix the root cause first: eliminate the cast, close the resource, remove the deprecated call, or restructure the code. Only suppress when the warning is a known false positive or the fix would be disproportionately invasive, and add a comment explaining why. In test code, suppression is more acceptable because mock frameworks and reflection-heavy helpers routinely trigger raw-type, resource, and boxing warnings that cannot be eliminated.

Useful commands:

- Repository build: `mvn -B package`
- Targeted module tests: `mvn -pl skyve-core -am test`
- Generated-domain refresh for the assembled app: run `mvn skyve:generateDomain` from `skyve-war/`
- Full verification including failsafe tests when relevant: `mvn -B verify -DskipIntegrationTests=false`
- Plugin help and targets: see [../../skyve-maven-plugin/readme.md](../../skyve-maven-plugin/readme.md)

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
