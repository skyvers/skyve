# Coverage Improvement Plan

**Goal:** Minimum 80% line coverage for each in-scope sub-project in the JaCoCo aggregate report (`skyve-coverage`) and at least 80% overall aggregate coverage across the combined testable surface. The admin module domain tests are included as they contribute to the aggregate and exercise framework code paths heavily.

**Measurement:** JaCoCo aggregate report produced by `skyve-coverage`. Success criteria are: (1) each in-scope sub-project reaches at least 80% line coverage in the aggregate report view, and (2) the overall combined aggregate reaches at least 80%. Tests in any module (including `skyve-war`) that call framework classes contribute to the aggregate.

**Out of scope:** `skyve-ejb` — ignore this module entirely. Do not write tests in it and do not count its classes against coverage targets.

**Autonomous execution runbook:** see `docs/ai/skyve-web-autonomous-coverage-plan.md` for the iterative AIOM loop and milestone gates used to drive `skyve-web` toward 80%.

## Autonomous Run Kickoff (31 May 2026)

- Mode: parallel hands-off AIOM coordinator workflow.
- Run ID: `aiom-20260531T073124Z`.
- Baseline verify completed with `mvn -Pcoverage -pl skyve-web -DskipIntegrationTests=true -DskipUnitTests=false verify`.
- Baseline metrics captured in state ledger:
    - LINE: 41.28% (`10005/24235`)
    - BRANCH: 36.20% (`3827/10572`)
    - INSTRUCTION: 39.88% (`41295/103542`)
- State file initialized and marked running: `docs/ai/skyve-web-autonomous-coverage-state.md`.

---

## Build Commands

```bash
# Full aggregate — the authoritative coverage measurement
mvn -pl skyve-core,skyve-ext,skyve-web clean compile -q
mvn -Pcoverage -pl skyve-coverage -am -DskipIntegrationTests=true -DskipUnitTests=false verify

# skyve-core only (fast iteration for pure unit tests)
mvn -pl skyve-core,skyve-ext clean compile -q
mvn -Pcoverage -pl skyve-core -DskipIntegrationTests=true -DskipUnitTests=false verify

# skyve-ext only
mvn -pl skyve-core,skyve-ext clean compile -q
mvn -Pcoverage -pl skyve-ext -DskipIntegrationTests=true -DskipUnitTests=false verify

# skyve-web only
mvn -pl skyve-core,skyve-ext,skyve-web clean compile -q
mvn -Pcoverage -pl skyve-web -DskipIntegrationTests=true -DskipUnitTests=false verify

# Run just skyve-war tests (fast cycle for H2-backed tests — no report, just test)
mvn -pl skyve-war -DskipIntegrationTests=true -DskipUnitTests=false test

# Quick aggregate CSV check after a full build
awk -F',' 'NR>1 {missed+=$8; covered+=$9} END {printf "AGGREGATE: %d/%d (%.1f%%)\n", covered, missed+covered, covered*100.0/(missed+covered)}' \
    skyve-coverage/target/site/jacoco-aggregate/jacoco.csv
```

HTML reports:
- `skyve-core/target/site/jacoco/index.html`
- `skyve-coverage/target/site/jacoco-aggregate/index.html` (authoritative)

**Important:** Always run `mvn -pl skyve-core,skyve-ext,skyve-web clean compile` before a coverage build. Eclipse-compiled class files cause `bad class file` errors.

---

## Test Placement Rules

| Condition | Where to write the test |
|---|---|
| Pure Java — no persistence, no metadata loading, no CDI | `skyve-core/src/test/java/` or `skyve-ext/src/test/java/` |
| JSF converters (`getAsString`/`getAsObject`) | `skyve-web/src/test/java/` — no servlet needed |
| JSF chart config renderers (`charts/config`) | `skyve-web/src/test/java/` — use `new MockFacesContext()` directly |
| `skyve-web` filter or non-CORE servlet class | `skyve-web/src/test/java/` — plain Mockito of Jakarta servlet interfaces |
| Uses `CORE.*`, `EXT.*`, or `AbstractPersistence` singletons | `skyve-war/src/test/java/` — extend `AbstractH2Test` or `AbstractSkyveTest` |
| Needs full Customer/Module/Document metadata graph | `skyve-war/src/test/java/modules/test/` — extend `AbstractSkyveTest` |
| JSF pipeline/component builders (needs FacesContext + CORE) | `skyve-war/src/test/java/` — extend `AbstractSkyveTest`; open a `FacesUtil.withSailFacesContextIfNeeded()` scope in `@BeforeEach` |
| Admin module domain/actions | `skyve-war/src/test/java/modules/admin/` — extend `AbstractSkyveTest` |

Tests in `skyve-war` that call into `skyve-core`, `skyve-ext`, or `skyve-web` classes count toward those modules' coverage in the aggregate.

See [test-patterns.md](test-patterns.md) for full patterns, naming conventions, and code examples.

---

## Execution Plan — Work Packages

Ordered by impact-to-effort ratio. Execute in sequence.

