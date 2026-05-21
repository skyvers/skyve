# Coverage Improvement Plan

**Goal:** 80% line coverage of the _testable_ framework surface area, measured by the JaCoCo aggregate report (`skyve-coverage`). The admin module domain tests are included as they contribute to the aggregate and exercise framework code paths heavily.

**Measurement:** JaCoCo aggregate report produced by `skyve-coverage`. Tests in any module (including `skyve-war`) that call framework classes contribute to the aggregate.

**Out of scope:** `skyve-ejb` — ignore this module entirely. Do not write tests in it and do not count its classes against coverage targets.

---

## Current Baseline (21 May 2026)

| Scope | Lines covered | Total lines | Coverage |
|-------|-------------|-------------|----------|
| Aggregate (all) | 48 037 | 101 768 | **47.2%** |
| Framework testable | 36 424 | 61 697 | **59.0%** |
| Framework skipped | 5 962 | 23 106 | 25.8% |
| Admin/module code | 5 651 | 16 965 | 33.3% |
| `skyve-core` standalone | 22 582 | 38 732 | 58.3% |
| `skyve-web` standalone | 2 350 | 23 761 | 9.9% |

**Primary target: 80% of fully testable surface = ~67 120 lines covered (gap from current: ~19 083 lines).**
**Intermediate target: 80% of framework testable (excluding partially-testable) = ~53 560 lines (gap: ~5 500 lines).**

The skip list has been significantly reduced (from ~31 400 to ~14 400 hard-skipped lines) after re-evaluation showed many packages are testable with existing H2, MockFaces, and Mockito infrastructure. See "Skip List" section for full details.

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
| JSF pipeline/component builders (needs FacesContext + CORE) | `skyve-war/src/test/java/` — extend `AbstractSkyveTest`; call `FacesUtil.setSailFacesContextIfNeeded()` in `@BeforeEach` |
| Admin module domain/actions | `skyve-war/src/test/java/modules/admin/` — extend `AbstractSkyveTest` |

Tests in `skyve-war` that call into `skyve-core`, `skyve-ext`, or `skyve-web` classes count toward those modules' coverage in the aggregate.

See [docs/test-patterns.md](test-patterns.md) for full patterns, naming conventions, and code examples.

---

## Skip List — Genuinely Untestable Packages

These packages require live infrastructure (browser, full metadata generation pipelines, or runtime contexts) that cannot be reproduced in CI. They are excluded from the 80% target.

### skyve-core (~7 100 lines)

| Package | Lines | Reason |
|---|---|---|
| `impl/generate` + `impl/generate/client/*` | ~6 500 | Code generators — emit source code from full customer/module/document metadata graphs; testing individual outputs is impractical without a running generator pipeline |
| `impl/sail/execution` | 182 | SAIL executor — drives a live browser via WebDriver |
| `metadata/sail/**` (execution types) | ~200 | SAIL step/interaction types that only execute in a browser context |

### skyve-ext (~3 700 lines)

| Package | Lines | Reason |
|---|---|---|
| `impl/generate/jasperreports` (generators) | ~2 500 | Report design generators — need JasperReports runtime + full metadata |
| `impl/generate/sail` | 252 | SAIL test generator — needs full metadata pipeline |
| Other small packages (domain number, persistence dialect internals) | ~950 | Hibernate dialect registration, low-level persistence internals |

### skyve-web (~3 600 lines)

| Package | Lines | Reason |
|---|---|---|
| `impl/sail/execution/*` | 1 585 | SAIL PrimeFaces/SmartClient execution — drives live browser |
| `impl/web/faces/views` (core managed bean lifecycle) | ~1 000 | Deep CDI lifecycle methods (`@PostConstruct`, PrimeFaces AJAX callbacks) that require a live CDI container + Faces lifecycle. Utility methods within these classes ARE testable — see "Partially Testable" below |
| `impl/web/faces/components` (rendering) | ~300 | Custom composite rendering (`encodeBegin`/`encodeEnd`) needs a live component tree |
| Other (SAIL mock internals) | ~700 | SAIL mock/interpret infrastructure used only by SAIL execution |

### Partially Testable — Target Opportunistically

These packages were previously fully skipped but contain significant testable logic. Cover the accessible portions; do not aim for 80% on these individually.

| Package | Total lines | Testable portion | How to test |
|---|---|---|---|
| `metadata/view/model/list` | 1 882 | ~1 200 | H2/AbstractSkyveTest — `DocumentQueryListModel`, `InMemoryListModel`, `Page`, filter classes |
| `impl/metadata/repository` | 2 036 | ~800 | H2/AbstractSkyveTest — router, module/document resolution (not bootstrap loader) |
| `impl/persistence` (core) | 880 | ~400 | H2 — `DocumentFilterImpl`, query builder utility methods |
| `metadata/sail/**` (data types) | 222 | ~150 | Plain JUnit — SAIL step/interaction data structures (constructors, getters) |
| `impl/generate/jasperreports` (helpers) | 685 | ~400 | Plain JUnit/Mockito — `ReportBand`, field definitions, non-generator utility classes |
| `impl/web/faces/views` (utility) | 566 | ~400 | Mockito mock of FacesView + H2 — utility getters, state management methods |
| `impl/web/faces/components` (utility) | 163 | ~100 | MockFaces — component property setup, non-rendering methods |

### Now Testable — Removed from Skip List

These packages were previously skipped but analysis shows they are fully testable with existing infrastructure:

| Package | Lines | Module | How to test |
|---|---|---|---|
| `impl/bizport` | 1 804 | skyve-ext | Already has tests! POI/CSV operations with test files; extend existing `POISheetTest` etc. |
| `impl/snapshot` | 778 | skyve-ext | Pure data structures + JSON parsing — plain JUnit |
| `impl/report/freemarker` | ~600 | skyve-ext | Freemarker directives with mocked `Environment` + H2 for `CORE` |
| `impl/archive/job` | 518 | skyve-ext | H2/AbstractSkyveTest — job execution with Quartz mocking |
| `impl/create` | 252 | skyve-ext | File system operations — plain JUnit + temp directories |
| `impl/content` + `ejb` + `rest` | 411 | skyve-ext | File system + CORE — H2/AbstractSkyveTest + temp directories |
| `impl/cdi` | 280 | skyve-ext | Thin proxies delegating to CORE — H2/AbstractSkyveTest |
| `impl/web/faces/actions` | 1 047 | skyve-web | `Mockito.mock(FacesView.class)` + AbstractSkyveTest — actions take FacesView as constructor param, not injected |
| `impl/web/faces/models` | 324 | skyve-web | Mockito + H2 — `SkyveLazyDataModel`, `SkyveDualListModelMap` |
| `impl/sail/mock` + `interpret` | 263 | skyve-web | Plain JUnit — mock infrastructure support code |
| `impl/web/service/rest` + `filter/rest` | 502 | skyve-web | Mockito servlet mocks — no different from other filter testing |
| `impl/web/faces` (top-level utilities) | 432 | skyve-web | Mockito servlet mocks + H2 for CORE-dependent utilities |

**Total reclassified as testable: ~7 211 lines (full) + ~3 450 lines (partial) = ~10 661 lines**

### Revised Summary

| Module | Total lines | Hard-skipped | Partially testable | Fully testable |
|---|---|---|---|---|
| Framework (`org/skyve`) | 84 803 | ~14 400 | ~3 450 | **~66 950** |
| Admin/modules | 16 965 | 0 | 0 | **~16 965** |
| **Grand total** | 101 768 | ~14 400 | ~3 450 | **~83 900** |

**Revised 80% target: 80% of fully testable = 67 120 lines (gap from current 48 037: ~19 083 lines).**
**Conservative target: 80% of (fully testable − partially testable) = ~53 560 lines (gap: ~5 500 lines) — achievable in Phases 1–2.**

---

## Execution Plan — Work Packages

Ordered by impact-to-effort ratio. Execute in sequence.

---

### Phase 1 — Highest-Value Web Layer (target: +8 000 lines)

These are the largest testable packages with proven patterns.

#### WP-1: SmartClient Service Layer

| | |
|---|---|
| **Package** | `org/skyve/impl/web/service/smartclient` |
| **Missed** | 5 471 lines (3.2% covered) |
| **Test location** | `skyve-war/src/test/java/org/skyve/impl/web/service/smartclient/` |
| **Base class** | `AbstractSkyveTest` |
| **Template** | `ViewJSONManipulatorTest` (already exists and works) |
| **Sessions** | 3–4 (split by class group) |

**Classes to cover:**
- `SmartClientViewRenderer` — largest single class; drives view JSON generation
- `SmartClientListServlet` — list data serving
- `SmartClientEditServlet` — edit data serving
- `SmartClientGeneratorServlet` — metadata generation for the client
- `ViewJSONManipulator` — partially covered; extend existing tests
- Field/column/lookup definition builders

**Strategy:**
1. Load `AllAttributesPersistent` document from the test module (has every field type).
2. Instantiate the renderer/manipulator with the real document and a mock `HttpServletResponse`.
3. Call rendering methods and assert JSON output structure.
4. One test per field type / widget branch.

**Expected yield:** ~3 500 covered lines.

---

#### WP-2: JSF Component Builders

| | |
|---|---|
| **Package** | `org/skyve/impl/web/faces/pipeline/component` |
| **Missed** | 2 201 lines (32.8% covered) |
| **Test location** | `skyve-war/src/test/java/org/skyve/impl/web/faces/pipeline/` |
| **Base class** | `AbstractSkyveTest` |
| **Sessions** | 2–3 (split by widget type group) |

**Setup:**
```java
@BeforeEach
void setUpFaces() {
    FacesUtil.setSailFacesContextIfNeeded();
}
@AfterEach
void tearDownFaces() {
    FacesUtil.resetSailFacesContextIfNeeded();
}
```

**Key constraint:** `MockExpressionFactory` returns null for EL expressions. Assert component type and static properties, not evaluated EL values.

**Strategy:** One test per widget type method in `ComponentBuilder`. Call each method with real metadata, assert the returned `UIComponent` type and key non-null properties.

**Expected yield:** ~1 500 covered lines.

---

#### WP-3: Pipeline Layout + Orchestration

| | |
|---|---|
| **Package** | `impl/web/faces/pipeline` + `pipeline/layout` |
| **Missed** | 1 843 lines (4.4% combined) |
| **Test location** | `skyve-war/src/test/java/org/skyve/impl/web/faces/pipeline/` |
| **Base class** | `AbstractSkyveTest` + MockFaces |
| **Sessions** | 2 |

Same setup as WP-2. Test layout builder methods: `ResponsiveFormGrid`, `DeviceResponsiveLayoutBuilder`.

**Expected yield:** ~1 200 covered lines.

---

#### WP-4: Web Service Layer

| | |
|---|---|
| **Package** | `org/skyve/impl/web/service` (non-SmartClient, non-REST) |
| **Missed** | 2 189 lines (0.6% covered) |
| **Test location** | `skyve-war/src/test/java/org/skyve/impl/web/service/` |
| **Base class** | `AbstractSkyveTest` or `AbstractH2Test` |
| **Sessions** | 2 |

**Classes:** `ClientDocument`, servlet handlers, session management.

**Expected yield:** ~1 400 covered lines.

---

#### WP-5: Web Utilities (Non-Faces)

| | |
|---|---|
| **Package** | `org/skyve/impl/web` (top-level, excluding faces/*) |
| **Missed** | 1 753 lines (7.2% covered) |
| **Test location** | `skyve-war/src/test/java/org/skyve/impl/web/` |
| **Base class** | `AbstractH2Test` + Mockito |
| **Sessions** | 2 |

**Classes:** `WebUtil`, `AbstractWebContext`, `SkyveContextListener`, `UserAgent`.

**Expected yield:** ~1 100 covered lines.

---

### Phase 2 — Medium Framework Packages (target: +4 000 lines)

#### WP-6: Servlet Filters (Pure Mockito)

| | |
|---|---|
| **Package** | `impl/web/filter` + `filter/gzip` |
| **Missed** | 505 lines (0%) |
| **Test location** | `skyve-web/src/test/java/` (no CORE needed) |
| **Base class** | Plain JUnit 5 + Mockito |
| **Sessions** | 1 |

Mock `HttpServletRequest`, `HttpServletResponse`, `FilterChain`, `FilterConfig`. Exercise each filter's `doFilter` method.

**Expected yield:** ~400 lines.

---

#### WP-7: Spring Security Handlers

| | |
|---|---|
| **Package** | `impl/web/spring` |
| **Missed** | 521 lines (26.4%) |
| **Test location** | `skyve-war/src/test/java/org/skyve/impl/web/spring/` |
| **Base class** | `AbstractH2Test` |
| **Sessions** | 1 |

Auth success/failure handlers, security configuration paths.

**Expected yield:** ~350 lines.

---

#### WP-8: JSF Converters

| | |
|---|---|
| **Package** | `impl/web/faces/converters/*` |
| **Missed** | ~500 lines (13–69% across sub-packages) |
| **Test location** | `skyve-web/src/test/java/` |
| **Base class** | Plain JUnit 5 |
| **Sessions** | 1–2 |

Converters are pure `getAsString`/`getAsObject` methods. Most don't use FacesContext or UIComponent — pass null. One test class per converter sub-package.

**Sub-packages:**
- `converters/select` — 227 missed (largest)
- `converters/decimal` + `currency` — ~194 missed
- `converters/datetime` + `timestamp` — ~120 missed
- `converters/geometry` — 13 missed

**Expected yield:** ~400 lines.

---

#### WP-9: Binding and Expression Stack

| | |
|---|---|
| **Package** | `org/skyve/impl/bind` |
| **Missed** | 444 lines (77.3%) |
| **Test location** | `skyve-core/src/test/java/` + `skyve-war/src/test/java/` |
| **Base class** | Mixed — Mockito for pure paths, `AbstractH2Test` for CORE-dependent |
| **Sessions** | 2 |

Extend `BindUtilTest`. Cover remaining expression evaluator branches, bind/set paths, and validation methods that need a live customer/module/document.

**Expected yield:** ~300 lines (many remaining lines require full runtime — diminishing returns).

---

#### WP-10: Core Utilities

| | |
|---|---|
| **Package** | `org/skyve/util` + `org/skyve/impl/util` |
| **Missed** | ~600 lines (64–65%) |
| **Test location** | `skyve-core/src/test/java/` |
| **Base class** | Plain JUnit 5 |
| **Sessions** | 1–2 |

Pure Java helpers: `OWASP`, `TimeUtil`, `XMLMetaData`, `RuntimeCompiler`, `BeanValidator`, string/date/crypto utilities.

**Expected yield:** ~350 lines.

---

#### WP-11: Backup Infrastructure

| | |
|---|---|
| **Package** | `org/skyve/impl/backup` |
| **Missed** | 644 lines (61.8%) |
| **Test location** | `skyve-war/src/test/java/` |
| **Base class** | `AbstractSkyveTest` |
| **Sessions** | 2 |

Backup/restore pipelines. Needs multi-transaction H2 patterns.

**Expected yield:** ~400 lines.

---

#### WP-12: Job Scheduling

| | |
|---|---|
| **Package** | `org/skyve/impl/job` |
| **Missed** | 600 lines (9.6%) |
| **Test location** | `skyve-war/src/test/java/` |
| **Base class** | `AbstractSkyveTest` |
| **Sessions** | 2 |

Quartz integration, job lifecycle, `AbstractSkyveJob`.

**Expected yield:** ~350 lines.

---

#### WP-13: Metadata Resolution (User, Customer, View Component)

| | |
|---|---|
| **Packages** | `impl/metadata/user` + `customer` + `view/component` |
| **Missed** | 525 lines (53–68%) |
| **Test location** | `skyve-war/src/test/java/` |
| **Base class** | `AbstractSkyveTest` |
| **Sessions** | 1–2 |

Metadata resolution paths that need a live H2 customer/module graph.

**Expected yield:** ~350 lines.

---

#### WP-14: Cache + Security

| | |
|---|---|
| **Packages** | `impl/cache` + `impl/security` |
| **Missed** | 278 lines (14–43%) |
| **Test location** | `skyve-ext/src/test/java/` or `skyve-war/` |
| **Base class** | Mockito or `AbstractH2Test` |
| **Sessions** | 1 |

**Expected yield:** ~180 lines.

---

#### WP-15: Chart + Comparison Models

| | |
|---|---|
| **Package** | `metadata/view/model/chart` |
| **Missed** | 191 lines (61.3%) |
| **Test location** | `skyve-core/src/test/java/` |
| **Base class** | Plain JUnit 5 |
| **Sessions** | 1 |

Pure Java metadata model types — chart bucket types, comparison models.

**Expected yield:** ~150 lines.

---

### Phase 3 — Admin Module (target: +5 000 lines)

#### WP-16: Admin Domain Beans

| | |
|---|---|
| **Package** | `modules/admin/domain` |
| **Missed** | 2 998 lines (46.7%) |
| **Test location** | `skyve-war/src/test/java/modules/admin/` |
| **Base class** | `AbstractSkyveTest` |
| **Sessions** | 2–3 |

Generated domain beans. Use `AbstractDomainTest<T>` where available (auto-exercises all properties). Otherwise write simple getter/setter/enum tests.

**Expected yield:** ~2 000 lines.

---

#### WP-17: Admin Actions + Bizlets

| | |
|---|---|
| **Packages** | `modules/admin/*/actions` + bizlets |
| **Missed** | ~2 800 lines (0–38%) |
| **Test location** | `skyve-war/src/test/java/modules/admin/` |
| **Base class** | `AbstractSkyveTest` |
| **Sessions** | 3–4 |

Action invocation with `DataBuilder` fixtures. Key areas:
- `DataMaintenance/actions` — 242 missed
- `ReportTemplate/actions` — 211 missed
- `ImportExport/actions` — 236 missed
- `ControlPanel/actions` — 275 missed
- `Tag/actions` — 106 missed

**Expected yield:** ~1 800 lines.

---

#### WP-18: Monitoring Dashboard Models

| | |
|---|---|
| **Package** | `modules/admin/MonitoringDashboard/models` |
| **Missed** | 641 lines (0%) |
| **Test location** | `skyve-war/src/test/java/modules/admin/` |
| **Base class** | `AbstractSkyveTest` |
| **Sessions** | 1 |

**Expected yield:** ~400 lines.

---

#### WP-19: Test + Kitchen Sink Domain

| | |
|---|---|
| **Packages** | `modules/test/domain` + `modules/kitchensink/domain` |
| **Missed** | 939 lines (20–66%) |
| **Test location** | `skyve-war/src/test/java/modules/test/` |
| **Base class** | `AbstractSkyveTest` |
| **Sessions** | 1 |

**Expected yield:** ~600 lines.

---

#### WP-20: WhoSin + Remaining Admin Domain

| | |
|---|---|
| **Packages** | `modules/whosin/domain` + remaining admin |
| **Missed** | ~600 lines |
| **Test location** | `skyve-war/src/test/java/modules/` |
| **Base class** | `AbstractSkyveTest` |
| **Sessions** | 1 |

**Expected yield:** ~400 lines.

---

### Phase 4 — Formerly-Skipped Packages (target: +6 000 lines)

These were previously on the skip list but are now confirmed testable.

#### WP-21: BizPort Excel/CSV I/O

| | |
|---|---|
| **Package** | `org/skyve/impl/bizport` |
| **Missed** | ~1 400 lines (already partially tested) |
| **Test location** | `skyve-ext/src/test/java/` + `skyve-war/` for CORE-dependent methods |
| **Base class** | Plain JUnit 5 (POI operations) + `AbstractSkyveTest` (StandardGenerator/Loader) |
| **Sessions** | 2–3 |

Already has `POISheetTest`, `POIWorkbookTest`, `DataFileFieldTest`. Extend these. For `StandardLoader`/`StandardGenerator` which use Customer/Module/Document, use H2 in `skyve-war`.

**Expected yield:** ~1 100 lines.

---

#### WP-22: Faces Actions (Mock FacesView)

| | |
|---|---|
| **Package** | `org/skyve/impl/web/faces/actions` |
| **Missed** | ~1 047 lines (0%) |
| **Test location** | `skyve-war/src/test/java/org/skyve/impl/web/faces/actions/` |
| **Base class** | `AbstractSkyveTest` + `Mockito.mock(FacesView.class)` |
| **Sessions** | 2–3 |

Actions take `FacesView` as a constructor parameter (not injected). Mock it with Mockito, set up return values for `getBean()`, `getWebContext()`, `getUxUi()`, etc. Call `callback()` and assert persistence side effects.

Key classes: `SaveAction`, `DeleteAction`, `EditAction`, `AddAction`, `RemoveAction`, `ZoomInAction`, `ZoomOutAction`.

**Expected yield:** ~700 lines.

---

#### WP-23: Snapshot + SmartClient Adapter

| | |
|---|---|
| **Package** | `org/skyve/impl/snapshot` |
| **Missed** | ~778 lines (0%) |
| **Test location** | `skyve-ext/src/test/java/` |
| **Base class** | Plain JUnit 5 |
| **Sessions** | 1 |

Pure data structures: `Snapshot`, `SnapshotFilter`, `SnapshotColumn`, `SmartClientSnapshotAdapter`. Test JSON round-trip, filter construction, sort management.

**Expected yield:** ~650 lines.

---

#### WP-24: List Models (DocumentQuery, InMemory)

| | |
|---|---|
| **Package** | `org/skyve/metadata/view/model/list` |
| **Missed** | ~1 200 lines (testable portion) |
| **Test location** | `skyve-war/src/test/java/` |
| **Base class** | `AbstractSkyveTest` |
| **Sessions** | 2 |

Test `DocumentQueryListModel`, `InMemoryListModel`, `Page`, `Filter` classes with real H2 data. Use DataBuilder to create test documents, then exercise the model's `fetch()`, `getPage()`, filter/sort operations.

**Expected yield:** ~900 lines.

---

#### WP-25: Freemarker Report Directives

| | |
|---|---|
| **Package** | `org/skyve/impl/report/freemarker` |
| **Missed** | ~600 lines |
| **Test location** | `skyve-war/src/test/java/` (CORE-dependent) or `skyve-ext/src/test/java/` (pure) |
| **Base class** | `AbstractSkyveTest` + mocked Freemarker `Environment` |
| **Sessions** | 1–2 |

Test `FormatDirective`, `DisplayNameDirective`, `ContentDirective`, `ImageDirective` with mocked `TemplateModel` parameters and a live CORE for binding resolution.

**Expected yield:** ~400 lines.

---

#### WP-26: CDI Proxies + Content Manager

| | |
|---|---|
| **Packages** | `impl/cdi` + `impl/content` |
| **Missed** | ~690 lines |
| **Test location** | `skyve-war/src/test/java/` |
| **Base class** | `AbstractSkyveTest` (provides CORE) |
| **Sessions** | 1–2 |

CDI proxies are thin delegation — call each method and verify it delegates to CORE correctly. Content manager helpers: test file path construction, metadata JSON writing with temp directories.

**Expected yield:** ~500 lines.

---

#### WP-27: REST Services + Filters

| | |
|---|---|
| **Packages** | `impl/web/service/rest` + `impl/web/filter/rest` |
| **Missed** | ~502 lines |
| **Test location** | `skyve-web/src/test/java/` or `skyve-war/` |
| **Base class** | Mockito (servlet mocks) + `AbstractSkyveTest` |
| **Sessions** | 1 |

Mock `HttpServletRequest`/`HttpServletResponse`/`FilterChain`. Exercise filter chains and REST endpoint routing.

**Expected yield:** ~350 lines.

---

#### WP-28: Faces Models + SAIL Mock + Faces Utilities

| | |
|---|---|
| **Packages** | `impl/web/faces/models` + `impl/sail/mock` + `impl/web/faces` (top-level utils) |
| **Missed** | ~1 019 lines |
| **Test location** | `skyve-war/src/test/java/` |
| **Base class** | Mixed — Mockito + `AbstractSkyveTest` |
| **Sessions** | 1–2 |

`SkyveLazyDataModel`, `SkyveDualListModelMap`, `BeanMapAdapter` — test data model contract methods. SAIL mock classes are trivial infrastructure. `FacesWebContext` utility methods testable with mocked servlet objects.

**Expected yield:** ~700 lines.

---

#### WP-29: Archive Jobs + Create Utility

| | |
|---|---|
| **Packages** | `impl/archive/job` + `impl/create` |
| **Missed** | ~770 lines |
| **Test location** | `skyve-war/src/test/java/` (archive) + `skyve-ext/src/test/java/` (create) |
| **Base class** | `AbstractSkyveTest` + temp dirs |
| **Sessions** | 1–2 |

Archive jobs: test job execution logic with H2 data. Create utility: test project file structure generation with temp directories.

**Expected yield:** ~550 lines.

---

### Phase 5 — Framework Tail + Hardening (target: +2 500 lines)

#### WP-30: Hibernate Persistence (push past 90%)

| | |
|---|---|
| **Package** | `impl/persistence/hibernate` |
| **Missed** | 399 lines (81.8%) |
| **Sessions** | 1–2 |

#### WP-31: Archive List + Support

| | |
|---|---|
| **Packages** | `impl/archive/list` + `support` |
| **Missed** | 265 lines |
| **Sessions** | 1 |

#### WP-32: Repository Module + View (push to 90%+)

| | |
|---|---|
| **Packages** | `impl/metadata/repository/module` + `view` |
| **Missed** | 379 lines (80–82%) |
| **Sessions** | 1 |

#### WP-33: Module Query Metadata

| | |
|---|---|
| **Package** | `impl/metadata/module/query` |
| **Missed** | 165 lines (68.3%) |
| **Sessions** | 1 |

#### WP-34: EXT Facades + Top-Level

| | |
|---|---|
| **Package** | `org/skyve` (ext facades) |
| **Missed** | 153 lines (37.8%) |
| **Sessions** | 1 |

#### WP-35: Kickstart Utility

| | |
|---|---|
| **Package** | `impl/tools/kickstart` |
| **Missed** | 269 lines (0%) |
| **Sessions** | 1 |

#### WP-36: Mop-Up (all packages < 100 missed)

Drive every partially-covered package to 90%+. Includes:
- `impl/metadata/view` (206 missed, 80.8%)
- `org/skyve/util` (104 missed, 91.9%)
- `impl/util` (152 missed, 89.9%)
- Various small packages

**Sessions:** 2–3

---

## Projected Outcomes

| Milestone | Lines covered (est.) | Aggregate % | Notes |
|-----------|---------------------|-------------|-------|
| Current | 48 037 | 47.2% | |
| Phase 1 complete | ~56 700 | ~55.7% | +8 700 lines |
| Phase 2 complete | ~60 000 | ~58.9% | +3 300 lines |
| Phase 3 complete | ~65 200 | ~64.1% | +5 200 lines |
| Phase 4 complete | ~71 050 | ~69.8% | +5 850 lines (formerly-skipped) |
| Phase 5 complete | ~73 550 | ~72.3% | +2 500 lines |

**To reach 80% aggregate (81 414 lines) from Phase 5:**
- Gap: ~7 900 additional lines
- Partially-testable packages (~3 450 lines available)
- Deeper coverage of already-tested packages (push 70% → 90%)
- Additional admin module depth
- More achievable than previously estimated

---

## MockFacesContext Infrastructure (Reference)

Skyve ships a headless Faces mock stack in production code (`impl/sail/mock`):
- `MockFacesContext` — headless FacesContext
- `MockApplication` — registers all PrimeFaces component types
- `MockELContext` + `MockExpressionFactory` — returns null for all EL

Install via `FacesUtil.setSailFacesContextIfNeeded()` (thread-local). Remove via `FacesUtil.resetSailFacesContextIfNeeded()`.

**Critical:** `AbstractFacesBuilder` initializes FacesContext-dependent fields at construction time. `setSailFacesContextIfNeeded()` MUST be called in `@BeforeEach` BEFORE instantiating any builder.

This unlocks `impl/web/faces/pipeline/component` (3 953 lines), `pipeline/layout` (655 lines), and chart renderers for direct testing without a servlet container.

---

## Quick Reference — What Gets the Most Lines Per Session

| Work Package | Expected yield | Sessions | Lines/session |
|---|---|---|---|
| WP-1 SmartClient | ~3 500 | 3–4 | ~900–1 200 |
| WP-2 Component builders | ~1 500 | 2–3 | ~500–750 |
| WP-3 Pipeline/layout | ~1 200 | 2 | ~600 |
| WP-4 Web service | ~1 400 | 2 | ~700 |
| WP-5 Web utilities | ~1 100 | 2 | ~550 |
| WP-16 Admin domain | ~2 000 | 2–3 | ~700–1 000 |
| WP-17 Admin actions | ~1 800 | 3–4 | ~450–600 |
| WP-21 BizPort | ~1 100 | 2–3 | ~370–550 |
| WP-22 Faces Actions | ~700 | 2–3 | ~230–350 |
| WP-23 Snapshot | ~650 | 1 | ~650 |
| WP-24 List Models | ~900 | 2 | ~450 |
| WP-6 Filters | ~400 | 1 | ~400 |
| WP-8 Converters | ~400 | 1–2 | ~200–400 |

**Best lines-per-session:** WP-1, WP-16, WP-23 (Snapshot). Start with WP-1.

---

## Agent Execution Protocol

### Session scope

Each session targets **one work package** (or a portion of a large one). Do not mix work packages in a session.

### Pre-flight

1. **Read docs:** `docs/learnings.md`, `docs/test-patterns.md`, this file.
2. **Clean compile:** `mvn -pl skyve-core,skyve-ext,skyve-web clean compile -q`
3. **Pick target:** Next work package from the plan.
4. **Baseline coverage:** Run module-level JaCoCo to identify exact uncovered lines.

### Per-class loop

1. **Read production source** — identify uncovered branches.
2. **Find existing tests** — add to them.
3. **Write tests** — one per branch, follow [test-patterns.md](test-patterns.md).
4. **Compile and run** — fix failures.
5. **Verify coverage** — confirm lines are now covered.

### Post-session

```bash
mvn -pl skyve-war -DskipIntegrationTests=true test  # regression check
# Then full aggregate if completing a WP:
mvn -pl skyve-core,skyve-ext,skyve-web clean compile -q
mvn -Pcoverage -pl skyve-coverage -am -DskipIntegrationTests=true -DskipUnitTests=false verify -q
```

### Error recovery

| Problem | Fix |
|---|---|
| `bad class file` errors | `mvn -pl skyve-core,skyve-ext,skyve-web clean compile` |
| NPE using `CORE.*` | Move test to `skyve-war` with H2 base class |
| `@Inject` field null | Make test class `public` |
| 0% despite passing tests | Check `skyve-coverage/pom.xml` includes the module |
| OOM in skyve-war tests | Add `-DforkCount=1` |
| MockFaces NPE | `setSailFacesContextIfNeeded()` BEFORE builder instantiation |

### Rules

- Do not test skip-listed packages.
- Do not create duplicate test classes.
- Do not suppress failures.
- Do not refactor production code unless asked.
- Do not spend > 30 min on one resistant class — move on.
- After editing Java, run `get_errors` and clear all warnings.
