# Guidance for Project Agents

Skyve is a metadata-driven Java low-code framework implemented as a multi-module Maven repository. The source of truth is split between framework Java code, XML metadata, generated artefacts, and assembled runtime modules. Agent work must respect those boundaries.

## Required Reading

- [docs/learnings.md](docs/learnings.md) must always be read before starting work. It contains durable engineering guidance for this repository.
- [docs/plan.md](docs/plan.md) must be read when asked to plan or execute a milestone or sprint. It defines the planning workflow, milestone plan format, execution steps, and the current roadmap.
- [docs/test-patterns.md](docs/test-patterns.md) should be read before writing or modifying tests.
- [docs/coverage-plan.md](docs/coverage-plan.md) must be read before starting any coverage-improvement work. It defines the target, skip list, and ranked package priorities.
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

## Test Coverage

The active coverage plan is in [docs/coverage-plan.md](docs/coverage-plan.md). Read it before starting coverage-improvement work.

**Target:** 80% line coverage of the _testable_ surface area across `skyve-core`, `skyve-ext`, and `skyve-web`. Many packages are excluded (code generators, SAIL execution, view rendering infrastructure, full-runtime persistence) — see the skip list in the plan.

**Baseline (May 2026):** `skyve-core` 9.1% lines / 5.1% branches. `skyve-ext` 11.7% / 12.5%. `skyve-web` 7.8% / 5.2%.  
**Important:** After Eclipse builds the workspace, always run `mvn -pl skyve-core,skyve-ext clean compile` before a coverage build. Eclipse-compiled class files in `target/classes` cause Oracle javac to fail with `bad class file` errors. See [docs/learnings.md](docs/learnings.md).

### Coverage build commands

```bash
# skyve-core only (fast, no skyve-ext dependency)
mvn -Pcoverage -pl skyve-core -DskipIntegrationTests=true -DskipUnitTests=false verify

# Full aggregate (requires skyve-ext compile error to be fixed)
mvn -Pcoverage -pl skyve-coverage -am -DskipIntegrationTests=true -DskipUnitTests=false verify
```

HTML reports: `skyve-core/target/site/jacoco/index.html` and `skyve-coverage/target/site/jacoco-aggregate/index.html`.

### Where to write coverage tests

| Code under test | Test lives in | Base class |
|---|---|---|
| Pure Java — no persistence, no metadata loading | `skyve-core/src/test/java/` | Plain JUnit 5 or `@ExtendWith(MockitoExtension.class)` |
| Needs a live H2 session or `CORE.getPersistence()` | `skyve-war/src/test/java/modules/test/` | `AbstractSkyveTest` (truncate) or `AbstractSkyveTestDispose` (dispose) |
| Needs persistence but no rich domain graph | `skyve-war/src/test/java/util/` | `AbstractH2Test` |

Tests written in `skyve-war` that call `skyve-core` classes contribute to `skyve-core` coverage in the JaCoCo aggregate report. Use this freely for anything requiring H2 — the coverage credits flow correctly.

### Working order

Follow the tier ordering in [docs/coverage-plan.md](docs/coverage-plan.md):
1. Fluent builder packages (`metadata/view/fluent`, `metadata/module/fluent`, `metadata/model/document/fluent`, etc.) — highest line-count return for lowest effort.
2. Domain types and converters — small files, deterministic, no mocking.
3. `impl/util`, `impl/util/json`, `domain/messages` — utility classes.
4. `impl/bind` — extend existing `BindUtilTest`.
5. H2-backed tests in `skyve-war` for classes needing a live session.

## Cross-Cutting Invariants

- Metadata is primary. If behaviour is declared in customer/module/document/view/router metadata, change the metadata first and regenerate before patching runtime Java around it.
- Generated sources are derived artefacts. Do not hand-edit `src/generated/java` or `src/generatedTest/java` unless the user explicitly asks for generated output surgery and the regeneration path is unavailable.
- Customer overrides and multi-tenancy are first-class design constraints. Avoid introducing hard-coded behaviour that bypasses customer-specific metadata, repository lookup, or security rules.
- Keep dependency direction intact. Prefer changes in the highest layer that owns the concern instead of leaking web/runtime concerns downward into `skyve-core`.
- Backward compatibility matters. Skyve exposes framework APIs, metadata formats, generated-code conventions, and runtime behaviour used by downstream applications.
- Security, routing, validation, and persistence rules are part of the framework contract. Avoid bypassing framework abstractions with ad hoc SQL, servlet logic, or direct UI assumptions when an existing Skyve service or metadata mechanism already owns the behaviour.

## Javadoc Standards

Javadoc is the authoritative contract record for public API. Write it so that a human reading only the Javadoc — without the implementation — understands exactly what the method promises, what it costs, and what it might do unexpectedly.

### When to Write Javadoc

- Every public class, interface, enum, and annotation in `skyve-core` — these form the stable framework API.
- Every public and protected method or constructor in `skyve-core` and any module that other modules or downstream applications call directly.
- Package-level `package-info.java` when the package has a non-obvious purpose or threading model.
- Skip generated sources (`src/generated/java`, `src/generatedTest/java`). They are throwaway artefacts.
- Skip trivial JavaBean getters and setters unless the field carries a non-obvious invariant (e.g. lazy initialisation, thread confinement, `null` meaning "not yet resolved").

### Lead Sentence

Open with a short third-person verb phrase that states what the element does, not what it is.

```java
/** Returns the resolved customer configuration, loading it on first access. */
/** Persists all pending changes for the current thread's persistence context. */
/** Builds a fully-populated fixture graph suitable for domain round-trip tests. */
```

Never restate the element name: `/** Gets the name. */` adds nothing.

### Contracts and Invariants

Document preconditions, postconditions, and invariants — things that must be true before the call, after it, and throughout the object's lifetime.

```java
/**
 * Resolves and returns the binding value at {@code path} relative to {@code bean}.
 *
 * <p>Precondition: {@code path} must be a valid dot-separated Skyve binding expression;
 * behaviour is undefined for malformed paths.
 *
 * <p>Invariant: the returned value reflects the bean's state at the moment of the call;
 * subsequent mutations to {@code bean} are not reflected in the returned object.
 *
 * @param bean  the root domain object; must not be {@code null}
 * @param path  dot-separated binding expression; must not be {@code null} or empty
 * @return the resolved value, or {@code null} if any intermediate binding is {@code null}
 */
```

### Side Effects

Name every observable effect beyond the return value. Use a `Side effects:` paragraph for contractual effects and `@implNote` when the effect is an implementation detail a caller need not rely on.

```java
/**
 * Saves the document instance and flushes the current Hibernate session.
 *
 * <p>Side effects: writes to the database; invalidates the second-level cache entry
 * for this bean's entity type; publishes a {@code PostSaveEvent} on the Skyve event bus.
 *
 * @throws DomainException if a validation rule is violated before the flush
 */
```

Always be explicit about thread-local state mutations, cache writes, file system writes, and external service calls.

### Complexity

Document Big-O time and space when it is non-obvious or when a caller might accidentally embed the call in a loop.

```java
/**
 * Returns all documents accessible to {@code role} within {@code module}.
 *
 * <p>Complexity: O(d) time where d is the number of documents in the module;
 * results are not cached — avoid calling this inside a per-request loop.
 */
```

Use `O(1)`, `O(n)`, `O(n log n)`, `O(n²)` notation and qualify what `n` represents.

### Data Structure Choice

When a specific collection type matters to the caller — ordering, deduplication, identity semantics, modification rights — document it.

```java
/**
 * Returns the attributes declared on this document in declaration order.
 *
 * <p>Uses a {@link java.util.LinkedList} internally to allow O(1) insertion at either
 * end during metadata merging; callers that need random access should copy to an
 * {@link java.util.ArrayList}.
 *
 * @return a mutable, ordered list; never {@code null}
 */
```

If the caller must not modify the returned collection, say so explicitly.

### Threading

State the threading model whenever a class or method has non-trivial concurrency behaviour. Use one of these standard phrases in class-level Javadoc:

- *Thread-confined:* instances must not be shared across threads (e.g. `AbstractPersistence`).
- *Thread-safe:* all public methods may be called concurrently without external synchronisation.
- *Guarded by `<lock>`:* access to the named state is guarded by the named monitor.
- *Not thread-safe:* callers are responsible for external synchronisation.

For methods with specific threading constraints:

```java
/**
 * Executes the batch job step.
 *
 * <p>Threading: must be called from the job-execution thread. This method reads and
 * writes the thread-local persistence context; do not call from a fork/join worker
 * or a parallel-stream callback.
 */
```

Document `volatile` fields, lock ordering, and `ThreadLocal` ownership in `@implNote` when the detail is implementation-specific rather than part of the public contract.

### Tags Reference

| Tag | Use |
|-----|-----|
| `@param name` | Every non-obvious parameter; include null-safety and valid range |
| `@return` | Every non-void method; state null-safety and ownership |
| `@throws ExceptionType` | Every checked exception; important unchecked ones (e.g. `IllegalArgumentException`, `ObjectNotFoundException`) |
| `@implNote` | Implementation detail a maintainer should know but a caller need not rely on |
| `@implSpec` | Contract that subclasses or overriders must honour |
| `@since x.y` | New API added to a stable module; use the Skyve version string |
| `@see` | Related types or methods worth cross-referencing |
| `{@code ...}` | Inline code, type names, literals |
| `{@link ...}` | Cross-references to other types or members |

Omit `@param` and `@return` only when the name and lead sentence make their meaning completely unambiguous.

### What Not to Do

- Do not restate the method name or signature in prose.
- Do not add placeholder Javadoc (`@param name the name`).
- Do not document generated sources.
- Do not use HTML block elements (`<table>`, `<ul>`) unless a plain paragraph cannot express the structure.
- Do not describe the *how* (implementation steps) unless the algorithm choice itself is part of the contract.

## Performance and Coding Conventions

Skyve must be performant. Apply these rules to all production Java code; they are not optional style preferences.

### Threading

- All shared mutable state must be explicitly thread-safe. Choose `synchronized`, `volatile`, `java.util.concurrent` types, or immutability — pick the lightest mechanism that is correct and document the choice.
- Never silently share thread-local state across method boundaries. `ThreadLocal` fields must document their lifecycle: who initialises, who cleans up, and under what exception path.
- Prefer stateless collaborators. A class that holds no mutable instance state is thread-safe by construction.

### Complexity and Allocation

- Be Big-O aware. Before writing any loop over a collection, know what `n` is and whether the body has hidden O(n) or O(n log n) operations (e.g. `List.contains`, `String.format`, getter chains that recompute). Nested loops over metadata collections are a common source of O(n²) behaviour in Skyve.
- Do not allocate objects inside tight loops when a local reference declared outside the loop will do.
- Prefer pre-sized collections when the final size is known or estimable: `new ArrayList<>(size)`, `new HashMap<>(size * 4 / 3 + 1)`.
- Avoid autoboxing in hot paths. Use primitive arrays or `int`/`long` locals rather than `Integer`/`Long` wrappers when the value is only needed locally.

### References over Repeated Getter Calls

Assign the result of a getter (or any non-trivial accessor) to a local variable when it is used more than once in the same method. Repeated getter calls can hide computation, trigger lazy loading, or break consistency if the underlying state mutates mid-method.

```java
// Wrong — getAttributes() may be O(n) or trigger lazy init on every call
for (int i = 0; i < document.getAttributes().size(); i++) {
    process(document.getAttributes().get(i));
}

// Right — one call, one reference
List<Attribute> attributes = document.getAttributes();
int size = attributes.size();
for (int i = 0; i < size; i++) {
    process(attributes.get(i));
}
```

This also applies to chained calls: assign intermediate results to named locals so each accessor appears exactly once and the intent is readable.

### Loops over Streams

Prefer `for` loops over the Stream API in production framework code.

- `for` loops are easier to read, step through in a debugger, and reason about under exceptions.
- Streams introduce lambda allocation, capture overhead, and can obscure `throws` declarations.
- Use streams only when the pipeline genuinely simplifies the logic (e.g. `collect`, `flatMap` over nested structures) and the call site is not on a hot path.
- Never use a parallel stream unless you have measured that it helps and you have verified the pipeline is safe under concurrent execution.

```java
// Prefer
for (Module module : modules) {
    if (module.isActive()) {
        register(module);
    }
}

// Avoid in framework hot paths
modules.stream().filter(Module::isActive).forEach(this::register);
```

### Injection Agnosticism

Skyve framework classes must not depend on a specific injection container (CDI, Spring, Guice, etc.).

- Write classes so that all collaborators can be supplied via constructor or setter without a container. This keeps the class testable in plain JUnit without a full application context.
- Where existing code uses CDI or Spring annotations, leave them in place — do not gratuitously remove working wiring — but do not add new hard dependencies on a particular container API.
- If a collaborator is optional or environment-specific, guard access behind an interface or factory rather than an `@Inject` field.
- Introduce an explicit test seam (package-private setter or constructor overload) when a class must be instantiated directly in tests.

## Codebase Style

- Keep changes simple, explicit, and minimal. Reduce concept count before adding helpers, but do not duplicate logic when a clean shared abstraction already exists.
- Match the indentation style of the file you are editing. Do not introduce unindented blocks, mixed tab/space indentation, or inconsistent continuation indents.
- Prefer documenting invariants and contracts over writing commentary about obvious mechanics. Follow the Javadoc Standards section above.
- Name side-effecting methods so the mutation or external effect is obvious.
- When the framework already has a pattern for a concern, follow the pattern. In Skyve this often means changing metadata, generator inputs, extension points, or service abstractions instead of inserting special cases.
- Avoid introducing new dependencies or frameworks unless the existing stack is demonstrably insufficient.
- Preserve existing test style within the area you are touching. This repository currently contains both JUnit 4 and JUnit 5 tests.
- Do not delete seemingly-unused code until you understand whether it participates in generation, reflection, metadata lookup, CDI/Spring wiring, or customer override behaviour.

## Change Workflow

- Inspect the existing code, metadata, generated artefacts, and tests before designing a fix.
- If the task affects documents, modules, views, routes, or generated domain classes, identify the metadata source of truth and the generation step before editing anything.
- Prefer targeted Maven commands while iterating, then run broader validation when the change settles.
- If metadata has changed, regenerate before trusting compile or test results.
- If a prerequisite is wrong, fix the prerequisite cleanly instead of layering a workaround on top.
- After editing any Java file, call `get_errors` on that file and resolve every warning before declaring the work complete. The project uses committed `.settings/org.eclipse.jdt.core.prefs` files that configure Eclipse JDT warning levels; VS Code surfaces these through `get_errors`. Common categories to watch: method can be declared static, deprecated API, autoboxing, potential resource leak, unnecessary `@SuppressWarnings`, raw types, and unused imports.
- When suppressing a warning, use the narrowest possible scope: prefer a declaration-level annotation (e.g. on a local variable or catch parameter) first, then method level, then class level only when the warning genuinely applies to the majority of members. Never suppress at a broader scope than necessary.
- In production code (non-test), `@SuppressWarnings` is a last resort. Fix the root cause first — eliminate the cast, close the resource, remove the deprecated call, or restructure the code. Only suppress when the warning is a known false positive or the fix would be disproportionately invasive, and add a comment explaining why. In test code, suppression is more acceptable because mock frameworks and reflection-heavy helpers routinely trigger raw-type, resource, and boxing warnings that cannot be eliminated.

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

### JaCoCo Coverage Profile

Skyve uses an opt-in JaCoCo coverage profile for code coverage analysis. The JaCoCo plugin and coverage report generation are only activated when the Maven build is run with the `-Pcoverage` profile. This means:

- **Normal builds** (without `-Pcoverage`) do not run JaCoCo, so they are faster and do not generate coverage data or reports.
- **Coverage builds** (with `-Pcoverage`) enable JaCoCo and generate per-module and aggregate coverage reports.

To run a coverage build and generate reports, use:

	mvn -Pcoverage -pl skyve-coverage -am -DskipIntegrationTests=true -DskipUnitTests=false verify

This approach keeps CI and developer builds fast unless coverage is specifically required.

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
