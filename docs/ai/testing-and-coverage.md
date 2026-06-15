# Testing and Coverage Guidance

## Test Coverage

The active coverage plan is in [../coverage-plan.md](../coverage-plan.md). Read it before starting coverage-improvement work.

**Target:** 80% line coverage of the _testable_ surface area across `skyve-core`, `skyve-ext`, and `skyve-web`. Many packages are excluded (code generators, SAIL execution, view rendering infrastructure, full-runtime persistence); see the skip list in the plan.

**Baseline (May 2026):** `skyve-core` 9.1% lines / 5.1% branches. `skyve-ext` 11.7% / 12.5%. `skyve-web` 7.8% / 5.2%.

**Important:** After Eclipse builds the workspace, always run `mvn -pl skyve-core,skyve-ext clean compile` before a coverage build. Eclipse-compiled class files in `target/classes` cause Oracle javac to fail with `bad class file` errors. See [learnings.md](learnings.md).

### Coverage Build Commands

```bash
# skyve-core only (fast, no skyve-ext dependency)
mvn -Pcoverage -pl skyve-core -DskipIntegrationTests=true -DskipUnitTests=false verify

# Full aggregate (requires skyve-ext compile error to be fixed)
mvn -Pcoverage -pl skyve-coverage -am -DskipIntegrationTests=true -DskipUnitTests=false verify
```

HTML reports: `skyve-core/target/site/jacoco/index.html` and `skyve-coverage/target/site/jacoco-aggregate/index.html`.

### Where to Write Coverage Tests

| Code under test | Test lives in | Base class |
|---|---|---|
| Pure Java (no persistence, no metadata loading) | `skyve-core/src/test/java/` | Plain JUnit 5 or `@ExtendWith(MockitoExtension.class)` |
| Needs a live H2 session or `CORE.getPersistence()` | `skyve-war/src/test/java/modules/test/` | `AbstractSkyveTest` (truncate) or `AbstractSkyveTestDispose` (dispose) |
| Needs persistence but no rich domain graph | `skyve-war/src/test/java/util/` | `AbstractH2Test` |

Tests written in `skyve-war` that call `skyve-core` classes contribute to `skyve-core` coverage in the JaCoCo aggregate report. Use this freely for anything requiring H2; the coverage credits flow correctly.

### Working Order

Follow the tier ordering in [../coverage-plan.md](../coverage-plan.md):

1. Fluent builder packages (`metadata/view/fluent`, `metadata/module/fluent`, `metadata/model/document/fluent`, and so on); highest line-count return for lowest effort.
2. Domain types and converters; small files, deterministic, no mocking.
3. `impl/util`, `impl/util/json`, `domain/messages`; utility classes.
4. `impl/bind`; extend existing `BindUtilTest`.
5. H2-backed tests in `skyve-war` for classes needing a live session.

## Testing Expectations

- If you changed Java code, metadata, generators, or packaging behaviour, run the narrowest meaningful validation first and widen from there.
- `mvn -B package` from the repository root is the baseline CI-aligned build.
- Surefire unit tests run by default; failsafe integration tests are skipped by default via the root Maven properties unless explicitly enabled.
- JUnit 5 test methods must be instance methods, not `static`, even though Jupiter can execute static tests. Use `@SuppressWarnings("static-method")` on the method or class when Eclipse/JDT flags pure assertion tests.
- Some higher-cost test suites live under SAIL/integration-oriented packages. Run them when the touched behaviour warrants it, but do not claim they ran if you only executed unit/module tests.
- In H2-backed framework tests, prefer existing test bases such as `AbstractH2Test`, `AbstractDomainTest`, or `AbstractActionTest` when they fit the change.
- When creating real Skyve document instances, prefer the generated `DocumentName.newInstance()` path rather than calling constructors directly. In database-backed tests, prefer `DataBuilder` for valid fixture graphs when the surrounding code already follows that pattern.
- Split test intent clearly: use `*H2Test` for persistence/integration paths and `*Test` for pure unit/Mockito paths.
- For actions with web-context dependencies, test both web and non-web code paths when both can execute in production.
- For security or workflow-affecting changes, pair automated tests with explicit manual verification steps.
- Documentation-only changes do not require a full reactor build, but they should still be reviewed for command accuracy and consistency with the current reactor.

## Command Quick Reference

Use the narrowest command that proves the change, then widen only as needed.

### Focused test commands

```bash
# Single test class in a module
mvn -pl <module> -Dtest=<TestClass> test -DskipIntegrationTests=true -DskipUnitTests=false

# Multiple modules where dependencies are needed
mvn -pl <moduleA>,<moduleB> -am test -DskipIntegrationTests=true -DskipUnitTests=false
```

### Coverage commands

```bash
# Module-level coverage
mvn -Pcoverage -pl <module> -DskipIntegrationTests=true -DskipUnitTests=false verify

# Aggregate coverage
mvn -Pcoverage -pl skyve-coverage -am -DskipIntegrationTests=true -DskipUnitTests=false verify
```

### Clean-compile first (when Eclipse has built classes)

```bash
# Run before coverage if javac reports bad class file issues
mvn -pl skyve-core,skyve-ext clean compile
```

Use the clean-compile prerequisite when you see `bad class file` or unresolved compilation artifacts from mixed Eclipse/javac outputs.

## JaCoCo Coverage Profile

Skyve uses an opt-in JaCoCo coverage profile for code coverage analysis. The JaCoCo plugin and coverage report generation are only activated when the Maven build is run with the `-Pcoverage` profile. This means:

- **Normal builds** (without `-Pcoverage`) do not run JaCoCo, so they are faster and do not generate coverage data or reports.
- **Coverage builds** (with `-Pcoverage`) enable JaCoCo and generate per-module and aggregate coverage reports.

To run a coverage build and generate reports, use:

```bash
mvn -Pcoverage -pl skyve-coverage -am -DskipIntegrationTests=true -DskipUnitTests=false verify
```

This approach keeps CI and developer builds fast unless coverage is specifically required.
