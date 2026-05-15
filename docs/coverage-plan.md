# Coverage Improvement Plan

**Goal:** 100% line coverage of the _testable_ surface area across the entire project (`skyve-core`, `skyve-ext`, `skyve-web`). The skip list is aggressive — everything not skipped should be fully covered.  
**Measurement:** JaCoCo aggregate report produced by `skyve-coverage`. Tests in any module (including `skyve-war`) that call framework classes contribute to the aggregate.

---

## Current Baseline (May 2026)

| Module | Lines covered | Total lines | Line % | Branch % |
|--------|-------------|-------------|--------|----------|
| `skyve-core` (standalone) | 15 919 | 38 794 | 41.0% | 20.9% |
| `skyve-ext` (standalone) | 4 453 | 20 694 | 21.5% | 18.0% |
| `skyve-web` (standalone) | 2 262 | 26 368 | 8.6% | 5.8% |
| **Aggregate** (all test sources) | **42 450** | **104 499** | **40.6%** | **31.3%** |

The aggregate is significantly higher than summing standalone numbers because `skyve-war` H2 tests exercise `skyve-core` and `skyve-ext` code heavily (e.g. `impl/bind` jumps from 22.5% standalone to 61.0% aggregate).

### Packages already at 80%+

20 609 lines across ~80 packages are already above 80% coverage (89.8% average). These need no further work.

### Packages at 0%

17 327 lines across ~80 packages have zero coverage. Most of these are on the skip list (see below). The remainder represent the highest-value opportunities.

---

## Build Commands

```bash
# Full aggregate — the authoritative coverage measurement
mvn -Pcoverage -pl skyve-coverage -am -DskipIntegrationTests=true -DskipUnitTests=false verify

# skyve-core only (fast iteration for pure unit tests)
mvn -Pcoverage -pl skyve-core -DskipIntegrationTests=true -DskipUnitTests=false verify

# skyve-ext only
mvn -Pcoverage -pl skyve-ext -DskipIntegrationTests=true -DskipUnitTests=false verify

# skyve-web only
mvn -Pcoverage -pl skyve-web -DskipIntegrationTests=true -DskipUnitTests=false verify

# Run just skyve-war tests (fast cycle for H2-backed tests — no report, just test)
mvn -pl skyve-war -DskipIntegrationTests=true -DskipUnitTests=false test
```

HTML reports:
- `skyve-core/target/site/jacoco/index.html` (module only)
- `skyve-coverage/target/site/jacoco-aggregate/index.html` (aggregate — the authoritative number)

**Important:** After Eclipse builds the workspace, always run `mvn -pl skyve-core,skyve-ext clean compile` before a coverage build. Eclipse-compiled class files cause `bad class file` errors. See [docs/learnings.md](learnings.md).

---

## Test Placement Rules

See [docs/test-patterns.md](test-patterns.md) for the full decision tree, detection heuristics, and code examples. The summary:

| Condition | Where to write the test |
|---|---|
| Pure Java — no persistence, no metadata loading, no CDI | `skyve-core/src/test/java/...` |
| Pure Java in `skyve-ext` (utilities, pure formatters) | `skyve-ext/src/test/java/...` |
| JSF converters (`getAsString`/`getAsObject`) | `skyve-web/src/test/java/...` — no servlet needed |
| Uses `CORE.*`, `EXT.*`, or `AbstractPersistence` singletons | `skyve-war/src/test/java/` — extend `AbstractH2Test` or `AbstractSkyveTest` |
| Needs full Customer/Module/Document metadata graph | `skyve-war/src/test/java/modules/test/` — extend `AbstractSkyveTest` |
| Covers `skyve-war` admin module classes | `skyve-war/src/test/java/modules/admin/...` |

Tests in `skyve-war` that call into `skyve-core` or `skyve-ext` classes **count toward those modules' coverage** in the aggregate JaCoCo report — use them freely.

---

## Skip List — Untestable Packages

These packages cannot be meaningfully tested without infrastructure that is impractical to provide in CI (live browsers, full servlet containers, specific database vendors, external services). They are **excluded** from the 80% target.

### skyve-core (est. ~11 900 lines excluded)

| Package | Lines | Reason |
|---|---|---|
| `impl/generate/client` | 862 | Client code generator — needs full customer + metadata graph |
| `impl/generate/client/flutter` | 683 | Flutter code generator — same |
| `impl/generate/client/react` | 1 037 | React code generator — same |
| `impl/generate` | 3 975 | Domain generator — needs full metadata graph; entry point already covered by `OverridableDomainGeneratorTest` |
| `impl/sail/execution` | 182 | SAIL executor — needs live browser |
| `metadata/sail/**` (all 8 packages) | 422 | SAIL step/interaction types — same |
| `impl/persistence` | 880 | Hibernate session contracts — covered by `skyve-ext` persistence tests |
| `impl/persistence/hibernate/dialect` | 2 | Vendor-specific dialect stubs |
| `metadata/view/model/list` | 1 882 | List model — requires running query/persistence context beyond H2 |
| `impl/metadata/repository` | 2 854 | Core metadata repository loader — extremely complex bootstrap; partially tested via H2 but not targetable to 80% |

### skyve-ext (est. ~10 800 lines excluded)

| Package | Lines | Reason |
|---|---|---|
| `impl/generate/jasperreports` | 3 185 | Jasper code generator — needs full customer + template context |
| `impl/bizport` | 1 804 | BizPort Excel I/O — needs POI + full persistence |
| `impl/snapshot` | 778 | Snapshot service — needs full persistence + serialisation |
| `impl/report/freemarker` | 699 | Freemarker renderer — needs full report context |
| `impl/report/jasperreports` | 302 | Jasper renderer — same |
| `impl/archive/job` | 518 | Archive jobs — needs full persistence + scheduling |
| `impl/generate/sail` | 252 | SAIL generator — needs full metadata |
| `impl/create` | 252 | Document creation pipelines — runtime-only |
| `impl/content` | 232 | Content manager — needs filesystem/Lucene |
| `impl/content/ejb` | 65 | EJB content — needs EJB container |
| `impl/content/rest` | 114 | REST content — needs servlet context |
| `impl/generate/charts` | 186 | JFreeChart generator |
| `impl/web` | 64 | Web context — needs servlet |
| `impl/tools/jasperreports` | 31 | Report tools |
| `impl/geoip` | 42 | GeoIP — needs network/database |
| `impl/sms` | 8 | SMS stub |
| `impl/sail/execution` | 54 | SAIL execution |
| `impl/persistence/hibernate/dialect/mysqlbugfix` | 133 | MySQL-specific dialect fix |
| `impl/persistence` (ext portion) | 385 | Low-level persistence wiring — tested via Hibernate layer |
| `impl/domain/number` | 12 | Domain number sequence — needs live sequence |
| `impl/domain/types` | 286 | Domain type implementations — runtime-wired |
| `impl/metadata/repository` (ext portion) | 223 | Repository extension — needs full bootstrap |
| `impl/cdi` | 280 | CDI producers — tested implicitly via container wiring |
| `impl/report` | 61 | Report base — same |
| `impl/tag` | 144 | Tagging — needs full persistence |
| `impl/addin` | 27 | Add-in wiring |

### skyve-web (est. ~22 400 lines excluded)

| Package | Lines | Reason |
|---|---|---|
| `impl/web/service/smartclient` | 6 331 | SmartClient JSON service — needs full request context |
| `impl/web/service` | 2 262 | Web services — needs servlet |
| `impl/web/service/rest` | 208 | REST services — same |
| `impl/web/faces/pipeline/component` | 3 953 | JSF component rendering |
| `impl/web/faces/pipeline` | 1 921 | JSF pipeline |
| `impl/web/faces/pipeline/layout` | 655 | Layout rendering |
| `impl/web/faces/views` | 1 708 | JSF view beans |
| `impl/web/faces/actions` | 1 080 | JSF action beans |
| `impl/web/faces/components` | 530 | JSF components |
| `impl/web/faces` | 445 | Core faces classes |
| `impl/web/faces/models` | 357 | Faces models |
| `impl/web/faces/charts/config` | 71 | Chart config |
| `impl/web/faces/renderers` | 10 | Renderers |
| `impl/web/filter` | 270 | Servlet filters |
| `impl/web/filter/gzip` | 246 | GZIP filter |
| `impl/web/filter/rest` | 304 | REST filter |
| `impl/web` (web portion) | 1 962 | General web infrastructure |
| `impl/sail/execution` | 169 | SAIL execution |
| `impl/sail/execution/pf` | 1 102 | PrimeFaces SAIL |
| `impl/sail/execution/sc` | 529 | SmartClient SAIL |
| `impl/sail/interpret` | 33 | SAIL interpreter |
| `impl/sail/mock` | 230 | SAIL mocks |

### Summary

| Module | Total lines | Skipped lines | Testable lines |
|---|---|---|---|
| `skyve-core` | 38 794 | ~11 900 | **~26 900** |
| `skyve-ext` | 20 694 | ~10 800 | **~9 900** |
| `skyve-web` | 26 368 | ~22 400 | **~4 000** |
| **Total** | **85 856** | **~45 100** | **~40 800** |

**Target: 100% of 40 800 testable lines.**  
**Current aggregate covered in testable packages: ~24 900 lines.**  
**Gap to close: ~15 900 additional lines.**

---

## Tier 1 — skyve-core: Pure Unit Tests (no H2)

These packages need only plain JUnit 5 or Mockito in `skyve-core/src/test/java`. They represent the fastest path to coverage gains.

### Already done (>= 80% in aggregate) — no action needed

| Package | Aggregate % | Notes |
|---|---|---|
| `metadata/view/fluent` | 96.1% | All builders covered |
| `metadata/module/fluent` | 88.7% | All builders covered |
| `metadata/model/document/fluent` | 89.2% | All builders covered |
| `metadata/router/fluent` | 97.9% | Done |
| `metadata/customer/fluent` | 96.4% | Done |
| `metadata/behaviour/fluent` | 100% | Done |
| `domain/types` | 95.9% | Done |
| `domain/types/converters/*` | 96–100% | Done |
| `domain/types/formatters` | 93.1% | Done |
| `impl/metadata/model` | 95.2% | Done |
| `impl/metadata/model/document/field` | 98.1% | Done |
| `impl/metadata/model/document/field/validator` | 94.1% | Done |
| `impl/metadata/module/menu` | 95.6% | Done |
| `impl/metadata/repository/behaviour` | 96.3% | Done |
| `impl/metadata/repository/view/actions` | 96.8% | Done |
| `cache` | 96.2% | Done |
| `impl/util/json` | 85.2% | Done |

### Remaining work — below 80% in aggregate

| Package | Aggregate % | Lines remaining | Where | Difficulty |
|---|---|---|---|---|
| `impl/bind` | 61.0% | ~764 | `skyve-core` + `skyve-war` H2 | Medium — extend `BindUtilTest` |
| `util` | 61.7% | ~881 | `skyve-core` + `skyve-war` H2 | Medium — `Util`, `OWASP`, `Time` |
| `impl/util` | 68.1% | ~684 | `skyve-core` | Medium — string/date/crypto helpers |
| `util/test` | 75.9% | ~87 | `skyve-war` H2 | Low — `DataBuilder` edges |
| `impl/metadata/repository/customer` | 76.6% | ~61 | `skyve-war` H2 | Low |
| `impl/metadata/repository/document` | 77.2% | ~118 | `skyve-war` H2 | Medium |
| `impl/metadata/repository/module` | 77.2% | ~214 | `skyve-war` H2 | Medium |
| `impl/metadata/view/event` | 77.8% | ~8 | plain JUnit | Trivial |
| `impl/metadata/model/document` | 78.8% | ~104 | `skyve-core` | Medium |
| `domain/messages` | 77.7% | ~42 | `skyve-core` | Low |
| `impl/metadata/view` | 75.9% | ~259 | `skyve-war` H2 | Medium |
| `impl/metadata/repository/view/access` | 74.1% | ~22 | `skyve-core` | Low |
| `metadata/view/widget` | 66.7% | ~28 | `skyve-core` | Low |
| `metadata/view` | 70.3% | ~49 | `skyve-core` | Low |
| `impl/metadata/view/widget/bound` | 88.0% | ~15 | `skyve-core` | Low |
| `metadata/model/document` | 58.8% | ~61 | `skyve-core` | Medium |
| `metadata/module` | 60.7% | ~11 | `skyve-core` | Low |
| `impl/metadata/module/query` | 67.9% | ~167 | `skyve-war` H2 | Medium |
| `impl/metadata/view/component` | 53.7% | ~183 | `skyve-war` H2 | Medium |
| `impl/metadata/customer` | 52.3% | ~199 | `skyve-war` H2 | Medium |
| `impl/metadata/user` | 39.7% | ~328 | `skyve-war` H2 | Medium |
| `metadata/view/model/chart` | 54.5% | ~225 | `skyve-core` | Medium — chart model types |
| `metadata/view/model/map` | 58.5% | ~56 | `skyve-core` | Low — map model types |
| `metadata/view/model/comparison` | 44.2% | ~101 | `skyve-core` or H2 | Medium |
| `impl/domain` | 39.5% | ~133 | `skyve-war` H2 | Medium |
| `domain` | 54.5% | ~111 | `skyve-core` | Low |

### Priority order

1. **Small-gap packages first** (< 50 lines each): `impl/metadata/view/event`, `domain/messages`, `metadata/module`, `impl/metadata/repository/view/access`, `impl/metadata/view/widget/bound`, `metadata/view/widget` — finish them off completely.
2. **`impl/bind`** (764 lines): high value, complex branches — mix of Mockito in `skyve-core` and H2 tests in `skyve-war`.
3. **`util`** (881 lines): mix of pure helpers (`OWASP`, `Time`) and H2-dependent (`DataBuilder`).
4. **`impl/util`** (684 lines): mostly pure Java helpers in `skyve-core`.
5. **`impl/metadata/user`** (328 lines): needs full H2 session — user/role resolution.
6. **`impl/metadata/customer`** (199 lines): H2 — customer override resolution.
7. **Chart/map/comparison models** (382 lines): mostly pure Java metadata types.
8. **All remaining packages** — drive to 100% systematically, largest gap first.

---

## Tier 2 — skyve-ext: H2-Backed Tests in `skyve-war`

Most `skyve-ext` packages need a live Skyve session. Tests go in `skyve-war/src/test/java/`.

### Already done (>= 80%)

| Package | % | Notes |
|---|---|---|
| `nlp/cron` | 96.5% | Done |
| `nlp/cron/elementprovider/*` | 89–100% | Done |
| `impl/mail` | 84.9% | Done |

### Remaining work

| Package | Aggregate % | Lines remaining | Base class | Difficulty |
|---|---|---|---|---|
| `impl/persistence/hibernate` | 23.0% | ~1 695 | `AbstractSkyveTest` | Hard — core Hibernate layer; highest single value |
| `impl/backup` | 6.1% | ~1 586 | `AbstractSkyveTest` | Hard — backup/restore |
| `impl/job` | 1.7% | ~653 | `AbstractSkyveTest` | Medium — job scheduling |
| `impl/script` | 58.5% | ~334 | `AbstractH2Test` | Medium — Groovy execution |
| `impl/archive/list` | 59.7% | ~129 | `skyve-ext` unit test | Low — mostly pure logic |
| `impl/archive/support` | 2.9% | ~202 | `skyve-ext` unit test | Medium |
| `job` (ext entry points) | 48.9% | ~71 | mock-based | Low |
| `impl/util` (ext) | 36.0% | ~409 | `skyve-ext` unit test | Medium |
| `impl/dataaccess/sql` | 27.9% | ~227 | `AbstractH2Test` | Medium |
| `util` (ext) | 28.7% | ~736 | mixed | Medium — `CommunicationUtil`, `Thumbnail` |
| `impl/security` | 14.3% | ~156 | mock or H2 | Medium |
| `impl/cache` (ext) | 4.2% | ~295 | `skyve-ext` unit test | Medium |
| `org.skyve` (ext facades) | 8.1% | ~226 | H2 or mock | Medium |

### Priority order

1. **`impl/archive/list`** and **`job`** (200 lines combined): easiest wins, mostly pure logic.
2. **`impl/util`** (ext, 409 lines): utility helpers — many are pure Java.
3. **`impl/security`** (156 lines) and **`impl/cache`** (295 lines): medium complexity, mockable.
4. **`impl/persistence/hibernate`** (1 695 lines): the single highest-value package; already partially covered.
5. **`impl/backup`** (1 586 lines): high value but complex; needs multi-transaction patterns.
6. **`impl/job`** (653 lines): job scheduling — needs CDI + metadata.
7. **All remaining** — drive every testable package to 100%.

---

## Tier 3 — skyve-web: Converter Layer (Pure Unit Tests)

The only meaningfully testable surface in `skyve-web` without a servlet container is the JSF converter layer. Tests go in `skyve-web/src/test/java/`.

### Current state

| Package | % | Lines remaining |
|---|---|---|
| `converters/date` | 69.2% | ~20 |
| `converters/time` | 69.2% | ~16 |
| `converters/integer` | 69.2% | ~20 |
| `converters/datetime` | 69.2% | ~60 |
| `converters/timestamp` | 69.2% | ~60 |
| `converters/decimal` | 16.0% | ~142 |
| `converters/decimal/currency` | 0% | ~52 |
| `converters/select` | 0.7% | ~267 |
| `converters/geometry` | 0% | ~13 |
| `converters/lang` | 88.9% | ~1 |

**Total testable in skyve-web:** ~4 000 lines. Converter layer is ~1 100 of that.  
**Target:** 100% of all testable converter lines (~651 remaining).

### Packages with partial coverage (need servlet — lower priority)

| Package | % | Notes |
|---|---|---|
| `impl/web/spring` | 22.2% | Spring security config — partially testable with `@WebMvcTest` but heavy setup |
| `impl/web/faces/pipeline/component` | 32.4% | Component rendering — some paths may be testable with mock `FacesContext` |
| `impl/web/service/sse` | 79.4% | Nearly done — SSE push events |

---

## Tier 4 — skyve-war Admin Module Tests

The `modules.admin.*` and `modules.whosin.*` packages in the aggregate report show 0% because `skyve-war` domain/action classes have no dedicated tests yet. These are application-level classes, not framework code, but they contribute to the aggregate.

These should be covered after framework code is at target. Writing domain tests here using `AbstractDomainTest<T>` and action tests with `AbstractSkyveTest` is straightforward.

---

## Gap Analysis Summary

| What | Lines remaining | Effort |
|---|---|---|
| Small-gap packages (< 50 lines each, Tier 1) | ~200 | Low |
| `impl/bind` + `util` + `impl/util` (core) | ~2 329 | Medium-Hard |
| `impl/metadata/user` + `impl/metadata/customer` + view/component | ~710 | Medium |
| Chart/map/comparison models | ~382 | Medium |
| Other Tier 1 packages (repository, module/query, domain) | ~990 | Medium |
| skyve-ext easy (archive, job, util, cache, security) | ~1 133 | Medium |
| skyve-ext heavy (hibernate, backup, job scheduling) | ~3 934 | Hard |
| skyve-ext medium (script, dataaccess/sql, ext facades, ext util) | ~1 196 | Medium |
| skyve-web converters | ~651 | Low |
| **Total gap to 100%** | **~15 900** | |

---

## Working Order

1. **Quick wins first:** finish all small-gap packages completely (< 50 lines each). Get them to 100%.
2. **skyve-web converters:** 651 lines, all pure JUnit, highly mechanical — complete the whole layer.
3. **Core utilities:** `impl/bind`, `util`, `impl/util` — high traffic code, high value for regression safety. Drive to 100%.
4. **Metadata resolution:** `impl/metadata/user`, `impl/metadata/customer`, `impl/metadata/view/component` — requires H2 but well-bounded.
5. **Chart/map/comparison models:** mostly pure Java metadata types in `skyve-core`.
6. **skyve-ext easy tier:** archive/list, job facades, security, cache, ext utilities — many are mockable.
7. **skyve-ext heavy tier:** hibernate persistence layer, backup/restore, job scheduling — hardest but highest absolute line count.
8. **Admin module domain tests** (Tier 4) — after framework packages are complete.

---

## Agent Execution Protocol

This section is a step-by-step runbook for an unguided agent (cloud or CLI) executing coverage work. Follow it exactly.

### Session scope

Each agent session should target **one package** (or a small cluster of related packages totalling < 500 uncovered lines). Do not attempt multiple tiers or unrelated packages in one session. If the package has > 500 uncovered lines, split it into logical sub-tasks (e.g. by class).

### Pre-flight (once per session)

1. **Read required docs** — before any code work:
   - `docs/learnings.md`
   - `docs/test-patterns.md`
   - `docs/coverage-plan.md` (this file)

2. **Clean compile** — Eclipse class files corrupt coverage builds:
   ```bash
   mvn -pl skyve-core,skyve-ext clean compile -q
   ```

3. **Pick target package** — choose the next package from the Working Order above. Confirm it is NOT on the skip list.

### Per-package loop

Repeat for each package in the session:

#### Step 1: Identify uncovered lines

Run module-level coverage and parse the CSV:
```bash
# For skyve-core packages:
mvn -Pcoverage -pl skyve-core -DskipIntegrationTests=true -DskipUnitTests=false verify -q
awk -F',' '$2 == "org.skyve.impl.bind" {printf "%s.%s: missed=%s covered=%s (%.1f%%)\n", $2, $3, $8, $9, $9*100/($8+$9)}' \
    skyve-core/target/site/jacoco/jacoco.csv

# For skyve-ext packages:
mvn -Pcoverage -pl skyve-ext -DskipIntegrationTests=true -DskipUnitTests=false verify -q
awk -F',' '$2 == "org.skyve.impl.archive.list" {printf "%s.%s: missed=%s covered=%s (%.1f%%)\n", $2, $3, $8, $9, $9*100/($8+$9)}' \
    skyve-ext/target/site/jacoco/jacoco.csv
```

This gives you per-class coverage within the package. Focus on classes with the most missed lines.

#### Step 2: Read the production source

For each class with significant uncovered lines, read it fully:
```bash
find skyve-core/src/main/java -path "*impl/bind/BindUtil.java" -exec cat {} \;
```

Identify the uncovered branches: switches, if/else chains, `instanceof` dispatch, exception paths, null guards. These are what you need to exercise.

#### Step 3: Find existing tests

```bash
# Check skyve-core tests
find skyve-core/src/test/java -name "*BindUtil*"

# Check skyve-war tests (H2-backed tests that may already cover this class)
find skyve-war/src/test/java -name "*BindUtil*" -o -name "*Bind*Test*"
```

If a test class already exists for the target class, **add to it** — do not create a parallel class.

#### Step 4: Decide test placement

Apply the detection heuristic from [docs/test-patterns.md](test-patterns.md):

- Does the class use `CORE.*`, `EXT.*`, or `AbstractPersistence.get()`? → **skyve-war**, extend `AbstractH2Test` or `AbstractSkyveTest`.
- Is it pure Java (no singletons, no CDI, no metadata loading)? → **same module** as the production code (`skyve-core/src/test/java`, `skyve-ext/src/test/java`, or `skyve-web/src/test/java`).
- Does it need `@Inject`? → Test class must be `public`.

#### Step 5: Write the tests

Rules:
- One `@Test` per branch/arm. Name it after what it proves: `convertsNullToEmptyString()`, `throwsOnMalformedBinding()`.
- For dispatch trees (`switch`, `instanceof` chains), write one test per case.
- Use `@SuppressWarnings("static-method")` on test methods that don't use instance state.
- Never `new` a Skyve document — use `newInstance()` or mock.
- In H2 tests, use `DataBuilder` for fixtures.
- Follow the patterns in [docs/test-patterns.md](test-patterns.md) for Mockito, H2, and fluent builder tests.

#### Step 6: Compile the tests

```bash
# skyve-core tests
mvn -pl skyve-core test-compile -q

# skyve-war tests (if that's where you wrote them)
mvn -pl skyve-war test-compile -q

# skyve-ext tests
mvn -pl skyve-ext test-compile -q
```

Fix all compile errors before proceeding. Run `get_errors` on each new/modified file.

#### Step 7: Run the tests

```bash
# Run just the new test class (fast feedback)
mvn -pl skyve-core -Dtest="org.skyve.impl.bind.BindUtilTest" test

# Or for skyve-war:
mvn -pl skyve-war -Dtest="util.BindUtilH2Test" -DskipIntegrationTests=true test
```

All tests must pass. If a test fails, fix the test — do not move on with failures.

#### Step 8: Verify coverage improvement

Re-run coverage and check the same CSV query from Step 1. Confirm the missed-line count dropped. If the package is still not at 100%, identify remaining classes and loop back to Step 2.

#### Step 9: Run broader validation

After finishing a package cluster, run the full module test suite to catch regressions:
```bash
mvn -pl skyve-core -DskipIntegrationTests=true test
mvn -pl skyve-war -DskipIntegrationTests=true test
```

### Aggregate verification (after multiple sessions)

Periodically (e.g. after finishing a tier), run the full aggregate to confirm credits flow:
```bash
mvn -pl skyve-core,skyve-ext clean compile -q
mvn -Pcoverage -pl skyve-coverage -am -DskipIntegrationTests=true -DskipUnitTests=false verify -q
awk -F',' 'NR>1 {missed+=$8; covered+=$9} END {printf "AGGREGATE: %d/%d (%.1f%%)\n", covered, missed+covered, covered*100.0/(missed+covered)}' \
    skyve-coverage/target/site/jacoco-aggregate/jacoco.csv
```

### Error recovery

| Problem | Fix |
|---|---|
| `bad class file` errors during compile | `mvn -pl skyve-core,skyve-ext clean compile` — Eclipse left stale `.class` files |
| `NullPointerException` in test using `CORE.*` | Test is in the wrong module — move to `skyve-war` with H2 base class |
| `@Inject` field is `null` at runtime | Test class is not `public` — Weld CDI requires public classes |
| Test passes locally but class still shows 0% in report | Test is in a module not included in the aggregate — check `skyve-coverage/pom.xml` dependencies |
| Aggregate build fails in `skyve-ext` | Stale Eclipse classes — run `mvn -pl skyve-core,skyve-ext clean compile` first |
| `OutOfMemoryError` during skyve-war tests | Add `-DforkCount=1` to limit parallel forks |

### What NOT to do

- Do not write tests for packages on the skip list.
- Do not create a new test class when one already exists for the target class.
- Do not combine unrelated packages in one test class.
- Do not suppress test failures — fix the test or fix the production code (with the user's approval for production changes).
- Do not refactor production code to make it "more testable" unless explicitly asked.
- Do not attempt more than ~500 uncovered lines per session.
- Do not skip the compile step — a test that doesn't compile wastes the entire session.
