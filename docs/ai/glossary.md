# AI Glossary

## Bizlet
Skyve behavior extension point tied to document lifecycle and business rules.

## Extension
Skyve customization class used to extend generated/default behavior for documents, actions, or services.

## Generated Domain
Code generated from metadata definitions (documents, modules, views). Generated sources are derived artifacts and should not be hand-edited.

## Metadata Source of Truth
XML/module/customer/view/router definitions that declare behavior and structure; runtime code should align to metadata intent.

## H2 Test
Database-backed test using Skyve H2 test infrastructure to exercise persistence and metadata-aware runtime paths.

## JaCoCo Coverage Profile
Maven `-Pcoverage` profile that enables coverage instrumentation and report generation.

## Milestone Plan
Implementation-ready plan document, typically `docs/ai/plan_m{n}.md`, with scope, dependencies, compatibility, and validation.

## Persona
Task role used by an agent (`Coder`, `Tester`, `Documenter`, `Planner`) with explicit mission, constraints, and exit criteria.

## Reactor Build Graph
The module dependency/build order defined by the root `pom.xml`; authoritative for what participates in standard builds.

## Thread-Local Persistence Context
Per-thread persistence state used by Skyve runtime and tests; must be initialized and cleaned up correctly to avoid test contamination.
