# Coverage Improvement Plan

**Goal:** raise line coverage across the project to 80% of the _testable_ surface area.  
**Primary module:** `skyve-core` (baseline 9.2% lines / 5.0% branches — see below).  
**Secondary:** `skyve-war` handwritten tests can cover `skyve-core` code via the aggregate report.  

---

## Build Commands

```bash
# Coverage for skyve-core alone (fast, no skyve-ext dependency)
mvn -Pcoverage -pl skyve-core -DskipIntegrationTests=true -DskipUnitTests=false verify

# Full aggregate (requires skyve-ext compile error to be fixed first)
mvn -Pcoverage -pl skyve-coverage -am -DskipIntegrationTests=true -DskipUnitTests=false verify

# Run just skyve-war tests (fast cycle for H2-backed tests)
mvn -pl skyve-war -DskipIntegrationTests=true -DskipUnitTests=false test

# Run just skyve-core tests
mvn -pl skyve-core test
```

HTML reports land at:
- `skyve-core/target/site/jacoco/index.html`
- `skyve-coverage/target/site/jacoco-aggregate/index.html` (full aggregate, when it builds)

---

## Baseline (skyve-core, May 2026)

```
OVERALL: 9.2% lines (3 719 / 40 554)   5.0% branches (697 / 13 818)
```

The table below drove the tier assignments. Packages are sorted ascending by line coverage.

---

## Test Placement Rules

| Condition | Where to write the test |
|---|---|
| Pure Java — no persistence, no metadata loading, no CDI | `skyve-core/src/test/java/...` |
| Needs a live Skyve H2 session, `Customer`, `Module`, generated domain documents | `skyve-war/src/test/java/modules/test/` — extend `AbstractSkyveTest` |
| Needs persistence but no rich domain graph | `skyve-war/src/test/java/util/` — extend `AbstractH2Test` |
| Covers a `skyve-war` admin module class | `skyve-war/src/test/java/modules/admin/...` |

Tests in `skyve-war` that call into `skyve-core` classes count toward `skyve-core` coverage in the aggregate JaCoCo report — use them freely for anything that needs H2.

---

## Skip List

These packages are **excluded** from the 80% target. Do not invest effort in them unless the task specifically requires it.

| Package (relative to `org/skyve/`) | Lines | Reason |
|---|---|---|
| `impl/generate/client/flutter` | 1 040 | Flutter client code generator — needs full customer + metadata graph to exercise meaningfully |
| `impl/generate/client/react` | 1 040 | React client code generator — same |
| `impl/generate/client` | 1 090 | Shared client generator base — same |
| `metadata/view/model/list` | 1 912 | List model infrastructure — requires a running Skyve query/persistence context |
| `impl/sail/execution` | 180 | SAIL test executor — needs a live browser / UI context |
| `metadata/sail/**` | ~800 | SAIL language step/interaction types — same |
| `impl/metadata/repository` | 3 195 | Core metadata repository loader — requires full customer bootstrap; extremely complex seam |
| `impl/generate` | 4 482 | Domain generator — needs full metadata graph; `OverridableDomainGeneratorTest` already covers the entry point; lower-level branches are too expensive to mock |
| `impl/persistence` | 911 | Hibernate session layer — covered instead by `skyve-ext` persistence tests (once its compile error is fixed) |
| `impl/metadata/view` | 1 139 | View metadata rendering infrastructure — needs full view bootstrap |
| `impl/metadata/view/component` | 399 | Same |
| `impl/metadata/view/widget` | 374 | Same |
| `impl/metadata/view/widget/bound` | 125 | Same |
| `impl/metadata/view/widget/bound/input` | 472 | Same |
| `impl/metadata/view/widget/bound/tabular` | 335 | Same |
| `impl/metadata/view/container` | 270 | Same |
| `impl/metadata/view/container/form` | 147 | Same |
| `impl/metadata/view/reference` | 111 | Same |
| `impl/metadata/view/event` | 36 | Same |
| `impl/metadata/view/model/chart` | 101 | Chart model — needs full list/query context |
| `metadata/view/model/chart` | 472 | Same |
| `metadata/view/model/chart/colours` | 69 | Same |
| `metadata/view/model/chart/colours/rainbow` | 100 | Same |
| `metadata/view/model/map` | 133 | Map view model — needs spatial runtime |
| `metadata/view/model/comparison` | 179 | Comparison model — needs full persistence |
| `util/monitoring` | 493 | Monitoring utilities — runtime-only (JVM metrics, thread dumps) |
| `domain/app/admin` | 135 | Admin domain — generated, covered by generated domain tests in skyve-war |
| `impl/domain` | 232 | Internal domain base classes — covered via skyve-war H2 tests |
| `impl/metadata/behaviour` | 69 | Behaviour metadata — complex runtime wiring |
| `impl/metadata/repository/behaviour` | 54 | Same |
| `impl/web` | 46 | Web context — needs servlet container |
| `job` | 22 | Job interfaces — thin; covered via skyve-war |

**Estimated skippable lines: ~20 000.** The target 80% applies to the remaining ~20 500 testable lines.

---

## Tier 1 — Pure Unit Tests in `skyve-core`

No persistence, no CDI, no H2. All tests go in `skyve-core/src/test/java`. Use `@ExtendWith(MockitoExtension.class)` or plain JUnit 5.

### High ROI — fluent builder APIs

These packages are almost entirely builder methods (constructors + setters + `build()` calls). One `@Test` per method covers a setter and a getter in five lines. This is the single biggest leverage point.

| Package | Current | Target | Lines missed | Difficulty |
|---|---|---|---|---|
| `metadata/view/fluent` | 6.3% | 80% | 3 379 | Low — all builders |
| `metadata/module/fluent` | 18% | 80% | 871 | Low — all builders |
| `metadata/model/document/fluent` | 8.1% | 80% | 986 | Low — all builders |
| `metadata/router/fluent` | 0% | 80% | 143 | Low — all builders |
| `metadata/customer/fluent` | 0% | 80% | 246 | Low — all builders |
| `metadata/behaviour/fluent` | 0% | 80% | 76 | Low — all builders |

**Pattern:** instantiate the fluent object, call every `with*` / `set*` method once, assert the get returns the set value, and call `build()` to assert the result is non-null or correct. A single parameterised test class per fluent type is sufficient.

### High ROI — utility and bind

| Package | Current | Target | Lines missed | Difficulty |
|---|---|---|---|---|
| `impl/bind` | 5.3% | 80% | 2 036 | Medium — `BindUtil` has many branches; mock `Customer`/`Document`/`Module` |
| `impl/util` | 30.1% | 80% | 1 085 | Medium — mix of string, date, and crypto helpers |
| `impl/util/json` | 30.7% | 80% | 399 | Medium — JSON serialisation helpers |
| `util` | 7.5% | 80% | 1 208 | Medium — `Util`, `OWASP`, `Time`, `DataBuilder` utilities |

`BindUtil` already has `BindUtilTest` and `StashExpressionEvaluatorTest` as a starting point. Extend them — do not create parallel classes.

### Medium ROI — metadata model classes

| Package | Current | Target | Lines missed | Difficulty |
|---|---|---|---|---|
| `impl/metadata/model/document` | 15.5% | 80% | 424 | Medium |
| `impl/metadata/model/document/field` | 37.4% | 80% | 164 | Medium |
| `impl/metadata/model` | 36.7% | 80% | 136 | Low-Medium |
| `impl/metadata/model/document/field/validator` | 62.9% | 80% | 59 | Low — mostly done |
| `metadata/model/document/fluent` | 8.1% | 80% | 986 | Low — builders |
| `metadata/model/document` | 20.1% | 80% | 131 | Medium |
| `metadata/model` | 46.7% | 80% | 56 | Low |

### Medium ROI — domain types and converters

These are pure value types and formatting classes. Existing tests in `skyve-core/src/test` cover date/time/timestamp converters well. Fill the remaining gaps.

| Package | Current | Target | Lines missed | Difficulty |
|---|---|---|---|---|
| `domain/types/converters/decimal` | 4.8% | 80% | 197 | Low |
| `domain/types/converters/enumeration` | 0% | 80% | 46 | Low |
| `domain/types/converters/integer` | 3.2% | 80% | 90 | Low |
| `domain/types/converters` | 6.2% | 80% | 45 | Low |
| `domain/types/formatters` | 23% | 80% | 57 | Low |
| `domain/types` | 28.4% | 80% | 156 | Low-Medium |
| `impl/domain/types/jaxb` | 14.3% | 80% | 36 | Low |
| `domain/messages` | 34.2% | 80% | 185 | Low — message hierarchy, pure Java |

### Quick wins — nearly there

| Package | Current | Target | Lines missed | Difficulty |
|---|---|---|---|---|
| `impl/metadata/model/document/field/validator` | 62.9% | 80% | 59 | Low — `IntegerValidatorTest` etc. already exist |
| `domain/types/converters/time` | 68% | 80% | 8 | Trivial |
| `impl/metadata/repository/view/access` | 48.2% | 80% | 44 | Low |
| `impl/metadata` | 50% | 80% | 14 | Low |
| `metadata/user` | 44.3% | 80% | 73 | Low |
| `metadata/controller` | 26.7% | 80% | 107 | Low-Medium |

### Other pure-unit targets

| Package | Current | Target | Lines missed | Difficulty |
|---|---|---|---|---|
| `impl/metadata/repository/document` | 36.9% | 80% | 328 | Medium |
| `impl/metadata/repository/customer` | 14.6% | 80% | 216 | Medium |
| `impl/metadata/repository/view` | 22.5% | 80% | 93 | Low |
| `impl/metadata/repository/module` | 7.4% | 80% | 883 | Medium-Hard |
| `cache` | 11.5% | 80% | 138 | Medium |
| `impl/domain/number` | 0% | 80% | 86 | Low |
| `domain/number` | 0% | 80% | 5 | Trivial |
| `metadata/module/query` | 0% | 80% | 2 | Trivial |
| `metadata/module` | 0% | 80% | 29 | Low |
| `impl/metadata/module` | 0% | 80% | 154 | Medium |
| `metadata/module/menu` | 0% | 80% | 166 | Medium |
| `impl/metadata/module/menu` | 0% | 80% | 87 | Medium |
| `impl/metadata/repository/router` | 30.8% | 80% | 126 | Medium |

---

## Tier 2 — H2-Backed Tests in `skyve-war`

These classes need a running Skyve session, `CORE.getPersistence()`, or access to generated domain objects. Write tests in `skyve-war/src/test/java/` extending the appropriate base class.

| Package | Current | Target | Lines missed | Base class | Notes |
|---|---|---|---|---|---|
| `impl/metadata/user` | 4.8% | 80% | 531 | `AbstractSkyveTest` | User, Role, Group resolution — needs full metadata |
| `impl/metadata/customer` | 0% | 80% | 361 | `AbstractSkyveTest` | Customer loading and override resolution |
| `util/test` | 17.7% | 80% | 306 | `AbstractH2Test` | `TestUtil`, `DataBuilder` — already partly covered in `DataBuilderH2Test` |
| `impl/metadata/repository/router` | 30.8% | 80% | 126 | `AbstractSkyveTest` | Router loading and merge |
| `impl/util/json` | 30.7% | 80% | 399 | `AbstractH2Test` | Some JSON paths need a persistence context for bean serialisation |

---

## Tier 3 — skyve-ext

**Baseline (May 2026):** 11.7% lines (2 353 / 20 121)  12.5% branches (1 014 / 8 081)

The compile blocker (`Thumbnail.java` / `MimeType`) was caused by a stale Eclipse-compiled class file in `skyve-core/target/classes/`. The fix is: `mvn -pl skyve-core,skyve-ext clean compile` before any aggregate build. See [docs/learnings.md](learnings.md) for the full explanation.

Most tests for skyve-ext live or should live in `skyve-war/src/test/java/modules/test/` (extend `AbstractSkyveTest`) or `skyve-war/src/test/java/util/` (extend `AbstractH2Test`). Existing coverage for the Hibernate persistence layer is tracked in `/memories/repo/hibernate-coverage.md`.

### Quick wins (already close to 80%)

| Package | Current | Missed | Notes |
|---|---|---|---|
| `nlp/cron` | 96.5% | 8 | Trivial — add edge-case inputs |
| `nlp/cron/elementprovider/recurring` | 95.4% | 12 | Same |
| `nlp/cron/elementprovider/hour` | 89.7% | 8 | Same |
| `nlp/cron/elementprovider` | 100% | 0 | Done |
| `job` | 48.9% | 71 | Push to 80% with mock-based action tests |
| `impl/archive/list` | 59.7% | 129 | Mostly pure logic — extend with unit tests |
| `impl/script` | 58.5% | 334 | Groovy/script execution — partially testable with mock bindings |

### Medium targets (pure unit or mock-based)

| Package | Current | Missed | Difficulty | Notes |
|---|---|---|---|---|
| `impl/util` | 30.1% | 535 | Medium | Utility helpers — mostly pure Java |
| `impl/dataaccess/sql` | 27.9% | 227 | Medium | SQL data access helpers |
| `impl/security` | 14.3% | 156 | Medium | Security utilities |
| `impl/cache` | 4.2% | 295 | Medium | Cache wrappers |
| `impl/archive/support` | 2.9% | 202 | Medium | Archive support utilities |
| `org/skyve` (ext entry points) | 6.9% | 228 | Medium | Thin facade methods |
| `util` | 6.9% | 933 | Medium | `CommunicationUtil`, `Mail`, `Thumbnail` etc. — mock file I/O and mail seams |

### H2-backed targets in skyve-war

| Package | Current | Missed | Base class | Notes |
|---|---|---|---|---|
| `impl/persistence/hibernate` | 23.0% | 1 688 | `AbstractSkyveTest` | Core Hibernate session — heavy but high value; already partially covered per repo memory |
| `impl/backup` | 5.0% | 1 602 | `AbstractSkyveTest` | Backup/restore — needs full persistence context |
| `impl/job` | 1.7% | 653 | `AbstractSkyveTest` | Job scheduling — needs full metadata |

### Skip list (skyve-ext)

| Package | Lines | Reason |
|---|---|---|
| `impl/generate/jasperreports` | 3 184 | Jasper code generator — needs full customer + template context |
| `impl/bizport` | 1 801 | BizPort Excel import/export — needs POI + full persistence |
| `impl/snapshot` | 778 | Snapshot service — needs full persistence + serialisation context |
| `impl/report/freemarker` | 699 | Freemarker report renderer — needs full report context |
| `impl/report/jasperreports` | 302 | Jasper renderer — needs full report context |
| `impl/archive/job` | 518 | Archive jobs — needs full persistence |
| `impl/generate/sail` | 252 | SAIL generator — needs full metadata |
| `impl/create` | 252 | Document creation pipelines — runtime-only |
| `impl/content` | 230 | Content manager — needs filesystem/Lucene context |
| `impl/content/rest` | 114 | REST content endpoint — needs servlet context |
| `impl/generate/charts` | 186 | JFreeChart generator — needs chart runtime |
| `impl/web` | 64 | Web context — needs servlet container |
| `impl/tools/jasperreports` | 30 | Jasper tools — needs report runtime |
| `impl/geoip` | 42 | GeoIP lookups — needs network context |
| `impl/sms` | 8 | SMS stub |
| `impl/sail/execution` | 54 | SAIL execution — needs UI context |

---

## Tier 4 — skyve-web

**Baseline (May 2026):** 7.8% lines (2 060 / 26 318)  5.2% branches (540 / 10 466)

Almost all of skyve-web requires a live JSF / servlet container. The only immediately testable surface is the converter layer, which mirrors the `skyve-core` converter pattern.

### Quick wins (converter layer — pure unit tests in skyve-web)

| Package | Current | Missed | Notes |
|---|---|---|---|
| `impl/web/faces/converters/time` | 69.2% | 16 | Mirror `skyve-core` converter tests |
| `impl/web/faces/converters/date` | 69.2% | 20 | Same |
| `impl/web/faces/converters/integer` | 69.2% | 20 | Same |
| `impl/web/faces/converters/datetime` | 69.2% | 60 | Same |
| `impl/web/faces/converters/timestamp` | 69.2% | 60 | Same |
| `impl/web/faces/converters/lang` | 88.9% | 1 | Trivial |
| `impl/web/faces/converters/decimal` | 16.0% | 142 | Same pattern — more variants |

Tests for the converter layer can live in `skyve-web/src/test/java` as plain JUnit 5 — no servlet context needed since `getAsString`/`getAsObject` take a `FacesContext` that can be `null` or mocked.

### Skip list (skyve-web — all require servlet/JSF container)

| Package | Lines | Reason |
|---|---|---|
| `impl/web/service/smartclient` | 6 331 | SmartClient JSON service — needs full request context |
| `impl/web/service` | 2 259 | Web services — needs servlet context |
| `impl/web/faces/pipeline/component` | 3 953 | Component rendering — needs full JSF lifecycle |
| `impl/web/faces/pipeline/layout` | 655 | Layout rendering — same |
| `impl/web/faces/pipeline` | 1 920 | Full pipeline — same |
| `impl/web/faces/actions` | 1 080 | JSF action beans |
| `impl/web/faces/views` | 1 696 | JSF view beans |
| `impl/web/faces/components` | 527 | JSF components |
| `impl/web/faces` | 444 | Core faces classes |
| `impl/web/filter*` | ~820 | Servlet filters |
| `impl/sail/*` | ~2 000 | SAIL execution layers |
| `impl/web/jasperreports` | 67 | Report rendering |

---

## Working Order Recommendation

1. **Ensure a clean Maven compile before the coverage build** — run `mvn -pl skyve-core,skyve-ext clean compile` after Eclipse has been building. The Eclipse JDT compiler leaves class files in `target/classes` that Oracle javac rejects as "bad class files" during skyve-ext compilation. See [docs/learnings.md](learnings.md).
2. **Tier 1 fluent builders** — highest line-count return for lowest effort. Start with `metadata/view/fluent`, then `metadata/module/fluent`, `metadata/model/document/fluent`, `metadata/router/fluent`, `metadata/customer/fluent`.
3. **Domain types and converters** (core + web) — small files, deterministic, no mocking needed.
4. **`domain/messages`** — pure Java hierarchy, straightforward.
5. **`impl/util` and `impl/util/json`** (core) — medium complexity; check what `UtilImplTest` and `VariableExpanderTest` already cover before adding.
6. **`impl/bind`** (core) — extend existing `BindUtilTest`; mock `Customer`/`Module`/`Document` at the narrowest seam.
7. **Metadata model document and field packages** (core) — extend existing `FluentFieldTest`.
8. **skyve-ext `nlp/cron*`** — nearly done; trivial additions.
9. **skyve-ext `impl/util`, `impl/dataaccess/sql`, `impl/security`** — medium-complexity utility classes.
10. **Tier 2 / Tier 3 H2-backed tests** — run these in skyve-war; they require a live Skyve session.
11. **Reassess** — re-run coverage after each cluster of packages and reprioritise based on updated numbers.

---

## What "80%" Means in Practice

| Module | Total lines | Estimated skippable | Testable surface | Lines needed at 80% | Currently covered |
|---|---|---|---|---|---|
| `skyve-core` | 38 705 | ~20 000 | ~18 705 | ~14 964 | 3 509 (9.1%) |
| `skyve-ext` | 20 121 | ~12 500 | ~7 621 | ~6 097 | 2 353 (11.7%) |
| `skyve-web` | 26 318 | ~25 800 | ~518 | ~414 | 2 060 (7.8%)* |

*skyve-web's overall coverage is boosted by the converter tests already written; the testable surface outside the JSF container is small.

**Total lines to add across all three modules to hit 80% of testable surface: roughly 13 600.**

The skyve-core fluent builder packages alone (~5 800 missed, low difficulty) account for ~37% of the skyve-core work in the single easiest cluster.
