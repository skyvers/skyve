# Learnings

`docs/learnings.md` is for durable engineering wisdom that should survive refactors and apply across Skyve modules and agent implementations. If a lesson only applies to one document, generator, or package, it belongs closer to that code or in [docs/architecture.md](architecture.md).

## Scope Ladder

- Repo-wide and long-lived: keep it here.
- Architecture or policy for Skyve as a framework: put it in [docs/architecture.md](architecture.md).
- Module- or subsystem-specific guidance: keep it with module-local docs.
- Metadata-file rules: prefer comments or adjacent docs near the metadata source of truth.
- Symbol-local contract: put it in code docblocks near the class or method. Follow the Javadoc Standards section in [agents.md](../agents.md) for how to document side effects, complexity, data structure choice, and threading.
- Naming or API smell: prefer reshaping the API over writing more prose.

Quick test: if the insight remains true after moving packages or adding new modules, it belongs here. If it depends on the current implementation of one subsystem, it probably does not.

## Engineering Discipline

- Treat metadata as the primary source of truth for document shape, views, queries, and privileges.
- Treat generated classes as disposable outputs, never authoritative handwritten code.
- When behaviour is wrong, first classify the failure: metadata contract, generated artefact drift, or handwritten runtime logic.
- Prefer deterministic, auditable workflows over hidden side effects.
- Keep logs useful but safe; avoid logging secrets or full sensitive payloads unnecessarily.
- Every warning is a mismatch between intent and reality. Fix it immediately or stop and design a clean fix. After editing any Java file, call `get_errors` on it and clear all diagnostics before moving on. The project commits `.settings/org.eclipse.jdt.core.prefs` to each module so Eclipse JDT warning levels are consistent and surfaced by `get_errors` in VS Code.
- Do not guess. Inspect the code, metadata, generated artefacts, and runtime path before proposing a change.
- Derive contracts from essential data flow. Remove unused inputs and document the invariants that remain.
- Recover intent before deleting code. In Skyve, seemingly dead code may still be referenced by metadata, reflection, generation, CDI/Spring wiring, or customer override paths.
- Generalise local cleanups into pattern audits. If one metadata loader, extension point, or generated artefact is wrong, nearby code often has the same flaw.
- Fix prerequisites cleanly. If goal X needs metadata generation, repository validation, or packaging cleanup first, do that work properly.
- Do not defer quality fixes that expose broken invariants, stale generation, or runtime drift.
- Treat documentation silence as "no". Do not invent framework behaviour unless you are also updating the source-of-truth docs and tests.
- Look for an existing mechanism before adding a new one. In Skyve, the right answer is often "use metadata, generation, or an existing service hook".
- Operational logs are data stores too. Never persist secrets (reset tokens, one-time codes, magic links, credentials); redact by default and require explicit opt-in for sensitive payload storage.

## Skyve-Specific Learnings

- Never edit generated sources under `src/generated/java/**` or generated tests under `src/generatedTest/java/**`.
- For metadata changes, regenerate domain before trusting compile/test outcomes.
- When introducing a brand new document, sequence changes as:
  1. add document XML
  2. register it in module XML
  3. run generation
  4. only then reference it from other metadata/views/extensions
  This avoids early compile/classloading failures against not-yet-generated domain classes.
- When enum/view metadata changes can affect generation classloading, compile first, then generate:
  1. `mvn -B -DskipTests compile`
  2. `mvn -B skyve:generateDomain`
- Keep heavy orchestration in services/jobs; use Bizlets for lifecycle rules and metadata-adjacent behaviour.
- The root Maven reactor is the authoritative build graph. Do not assume every repository directory participates in CI just because it exists on disk.
- `skyve-core` should stay focused on contracts, metadata, and abstractions. Push concrete runtime integrations outward into `skyve-ext` or delivery-specific modules unless there is a strong architectural reason not to.
- Backward compatibility is part of the design. Changes to metadata interpretation, generator output, or framework APIs can break downstream applications without any compile failure in this repository.
- Mixed test generations are intentional for now. Preserve local conventions unless the task is specifically to modernize the test layer.
- Customer overrides, role-based security, and routing are framework features, not application special cases. Fixes that bypass them are usually wrong even when they appear smaller.
- When framework code persists records outside normal request/user flows, required identity fields must still be valid. Use a deterministic synthetic framework user when a real user context is missing or incomplete.
- For cross-cutting behaviour (normalisation, test overrides, dispatch logging), route through one shared service entry point so all call sites inherit the same guarantees.

## Architecture and Refactorability

- Prefer designs where metadata interpretation, decision logic, and effects are separable. This makes generation and testing cheaper.
- Keep code generation deterministic. If a generated output changes, the input or template should explain why.
- Design modules around ownership boundaries. `skyve-core` defines, `skyve-ext` implements, `skyve-web` delivers, `skyve-war` assembles.
- Refactor toward clearer boundaries as soon as you see leakage. Metadata concerns leaking into web code, or servlet concerns leaking into core, get more expensive the longer they stay.

## Hibernate Persistence Specifics

- `session.load(entityName, id, LockMode.PESSIMISTIC_WRITE)` **throws `ObjectNotFoundException`** when the row does not exist — it does not return null. `session.get(entityName, id)` returns null. For consistent "not found" semantics, catch `ObjectNotFoundException` in the pessimistic-lock retrieval path and normalise to null so the calling method's contract (e.g. `retrieveAndLock` throws `NoResultsException`) is enforced in one place.
- `HibernateQueryDelegate.list()` in projected mode populates a `TreeMap<String, Object>` using HQL return aliases as keys. If the HQL `SELECT` clause has no explicit `AS alias`, `query.getReturnAliases()` returns `null`, causing an immediate `NullPointerException` in `TreeMap.put()`. Always include explicit aliases when using `projectedResults()` or `projectedIterable()`.
- `HibernateQueryDelegate.execute()` has a subtle bug: the `if (value instanceof Geometry)` branch is not inside the preceding `else if (value.getClass().isArray())`, so for array parameters, `setParameterList` fires *and then* `setParameter` fires for the same name. This is a pre-existing defect to be aware of when testing that path.
- Stale compiled classes from incremental builds can mask real test failures. If a test aborts with `java.lang.Error: Unresolved compilation problem` rather than a test assertion failure, run `mvn clean install -DskipTests` on the affected module before diagnosing test logic.

## Code Conventions

- **`Persistence.save()` return value must always be captured.** `Persistence.save(bean)` follows JPA semantics: the returned reference may be a different (managed) instance from the argument. Always write `bean = persistence.save(bean)`, never the bare `persistence.save(bean)`. If the saved reference is not read again after the call, suppress SonarLint's `java:S1854` ("useless assignment") warning with `@SuppressWarnings("java:S1854")` at **method level** — not on the variable declaration, because SonarLint fires the issue on the assignment statement and method-level scope is the narrowest annotation target that covers it. Do not use `@SuppressWarnings("unused")` for this case: Eclipse JDT does not flag these as unused, so that annotation would itself be flagged as unnecessary.

## Testing and Coverage Infrastructure

- JaCoCo aggregate reports only include modules listed as Maven dependencies of the aggregate module. A module that runs tests but is not a dependency contributes nothing to the aggregate — even if it exercises the same framework classes. Add WAR modules with `<classifier>classes</classifier>` to the aggregate's dependency list to include their test execution.
- When writing coverage-improving tests for a class with a large dispatch tree (many `instanceof` branches, `AttributeType` conditionals, collection/array/scalar paths), model each independent arm as its own small `@Test` method with a descriptive name. Bundling them degrades diagnostic clarity and makes partial regressions harder to localise.
- Mock at the narrowest seam that proves the branch. Injecting a mock `NativeQuery` via `session.createNativeQuery(...)` stub is cheaper and more reliable than building SQL that triggers the same branch by accident during real execution.
- Thread-local state (`AbstractPersistence.threadLocalPersistence`) must be cleaned up in `finally` blocks. A leaked thread-local from a failing test will corrupt subsequent tests in the same JVM thread if the runner reuses threads.
- When a test base class does static bootstrap (`@BeforeAll` at class level), inject session-level state (mock Session, test user) per test method rather than per class. Per-test injection is cheaper to reset and avoids state leakage between test methods.
- **Eclipse / Maven class file conflict:** When Maven is run after Eclipse has been building the workspace, Eclipse-compiled `.class` files in `target/classes/` can cause Oracle javac to emit `bad class file: class file is invalid for class ...` errors for downstream modules. Eclipse's JDT compiler writes non-standard class file attributes that javac rejects. The symptom also includes paths like `/BCDEFGHIJK/java.desktop/...` appearing in error messages. Fix: run `mvn -pl <affected-modules> clean compile` (or `clean install -DskipTests`) to flush Eclipse artefacts and rebuild with javac. For this repo the affected modules are typically `skyve-core`, `skyve-ext` and `skyve-web`. This must be done before any aggregate coverage build after Eclipse has touched `target/classes`.
