# Coverage Improvement Plan

**Goal:** 100% line coverage of the _testable_ surface area across the entire project (`skyve-core`, `skyve-ext`, `skyve-web`). The skip list is aggressive — everything not skipped should be fully covered.  
**Measurement:** JaCoCo aggregate report produced by `skyve-coverage`. Tests in any module (including `skyve-war`) that call framework classes contribute to the aggregate.  
**Out of scope:** `skyve-ejb` — ignore this module entirely. Do not write tests in it and do not count its classes against coverage targets.

---

## Current Baseline (June 2026 — updated)

| Module | Lines covered | Total lines | Line % | Tests |
|--------|-------------|-------------|--------|-------|
| `skyve-core` (standalone) | 22 381 | 38 716 | **57.8%** | 7 985 |
| `skyve-ext` (standalone) | ~6 062 | 20 694 | **29.1%** | — |
| `skyve-web` (standalone) | 2 042 | 23 788 | 8.6% | — |
| **Aggregate** (all test sources) | **~47 000** | ~101 879 | **~46%** | — |

Previous baselines: `skyve-core` 9.1% (original May 2026), 47.1% (18 May 2026), 57.6–57.8% (current session).  
`skyve-ext` started at 11.7%, reached 29.1% in a previous session (not rebuilt this session).

The aggregate is significantly higher than summing standalone numbers because `skyve-war` H2 tests exercise `skyve-core` and `skyve-ext` code heavily (e.g. `impl/bind` jumps from 22.5% standalone to 61.0% aggregate).

### Recent Progress (Current Session)

- `skyve-core` standalone moved from **57.6% → 57.8%** (22,381/38,716 lines, 7,985 tests).
- Added tests across multiple files this session:
  - `TimeUtilTest` — 23-char ISO date without timezone (`parseISODateParsesNoTimezoneWithMillis`)
  - `BindUtilTest` — 5 `fromString` throw-path tests (date/time/datetime/timestamp with null Customer; Object unknown type) + 2 `toDisplay` branch tests (Boolean.FALSE, null)
  - `DynamicBeanTest` — `putAllDynamic(null)` and `equals(non-DynamicBean)` edge cases
  - `DocumentImplTest` — `InverseOne.getCardinality()`, `CollectionImpl.isRequired()`, `AssociationImpl.setRequiredBool()`
  - `FieldModelTest` — `Geometry.getDomainType()`, `Id.getDomainType()`
  - `CustomerImplTest` — `lastModifiedMillis`, `lastCheckedMillis` round-trips
  - `HorizontalAlignmentTest` — `toTextAlignmentString()` coverage
  - `SpacerTest`, `StaticImageTest`, `MapDisplayTest` — `setVisibleConditionName` / `getInvisibleConditionName`
  - `ButtonTest` — `getProperties()` non-null
  - `AbstractDataWidgetTest` — `getVisibleConditionName()` via `DataGrid`
  - `HH24_MITest` — `getFormat()` non-null (batch 3)
  - `DD_MM_YYYY_HH24_MITest` — `getI18nKey()` non-null (batch 3)
  - `MM_DD_YYYY_HH24_MITest` — `getI18nKey()` non-null (batch 3)
  - `DD_MM_YYYY_HH24_MI_SSTest` — `getI18nKey()` non-null (batch 3)
  - `DecimalTypesTest` — `decimal5PowReturnsCorrectResult()`
  - `JSONTest` — `constructorIsCallable()`
  - `AutomationContextTest` — `userAgentTypeIsMobileReturnsTrueForPhoneAndTablet()`

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

**Important:** After Eclipse builds the workspace, always run `mvn -pl skyve-core,skyve-ext,skyve-web clean compile` before a coverage build. Eclipse-compiled class files cause `bad class file` errors. See [docs/learnings.md](learnings.md).

---

## Test Placement Rules

See [docs/test-patterns.md](test-patterns.md) for the full decision tree, detection heuristics, and code examples. The summary:

| Condition | Where to write the test |
|---|---|
| Pure Java — no persistence, no metadata loading, no CDI | `skyve-core/src/test/java/...` |
| Pure Java in `skyve-ext` (utilities, pure formatters) | `skyve-ext/src/test/java/...` |
| JSF converters (`getAsString`/`getAsObject`) | `skyve-web/src/test/java/...` — no servlet needed |
| JSF chart config renderers (`charts/config`) | `skyve-web/src/test/java/...` — use `new MockFacesContext()` directly |
| `skyve-web` filter or non-CORE servlet class | `skyve-web/src/test/java/...` — plain Mockito of Jakarta servlet interfaces |
| `skyve-web` class that calls `CORE.*`, needs `User`/`Customer`/`Document` but no JSF container | `skyve-war/src/test/java/org/skyve/impl/web/...` — extend `AbstractH2Test` or `AbstractSkyveTest` |
| `skyve-web` JSF pipeline/component builders | `skyve-war/src/test/java/...` — extend `AbstractSkyveTest`; call `FacesUtil.setSailFacesContextIfNeeded()` in `@BeforeEach` **before** instantiating any builder |
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

### skyve-web (est. ~10 800 lines excluded)

**The `MockFacesContext` mechanism — why many JSF packages are now testable:**

Skyve ships its own headless Faces mock stack in production code (`impl/sail/mock`): `MockFacesContext`, `MockApplication` (registers all PrimeFaces component types so `createComponent()` works), `MockELContext`, and `MockExpressionFactory` (returns null for all EL expressions — components are created but expressions are not evaluated). `FacesUtil.setSailFacesContextIfNeeded()` installs this on the current thread; `FacesUtil.resetSailFacesContextIfNeeded()` removes it.

This is exactly what SAIL uses — which is why `impl/web/faces/pipeline/component` already has 32.4% aggregate coverage from SAIL scripts. Combining `AbstractSkyveTest` (H2 session for CORE singletons) with `FacesUtil.setSailFacesContextIfNeeded()` (mock FacesContext for component tree construction) unlocks the component builder pipeline, layout builders, and chart config renderers for direct unit testing. Tests go in `skyve-war/src/test/java/` and extend `AbstractSkyveTest`.

**Important:** `AbstractFacesBuilder` initializes `fc`, `a`, `ef`, `elc` as field initializers at instantiation time — `setSailFacesContextIfNeeded()` must be called in `@BeforeEach` **before** any builder is instantiated. Failure to do so means `FacesContext.getCurrentInstance()` returns null and the fields NPE on first use.

**About the Mojarra JAR in `skyve-war` test scope:** `org.glassfish:jakarta.faces:4.0.11` is present for omnifaces CDI bootstrap, not for writing `FacesContext`-backed tests. Creating a real Mojarra `FacesContext` requires a `ServletContext` + `HttpServletRequest` — no simpler than running a container. Use the existing `MockFacesContext` stack instead.

**Still genuinely untestable** (require CDI JSF scoping, full servlet lifecycle, or a live browser):

| Package | Lines | Reason |
|---|---|---|
| `impl/web/faces/views` | 1 708 | CDI `@SessionScoped`/`@RequestScoped` managed beans — require JSF CDI scope to inject |
| `impl/web/faces/actions` | 1 080 | Action beans tightly coupled to `FacesView` CDI bean and require full persistence + CDI wiring |
| `impl/web/faces/components` | 530 | JSF custom composite components — need live component tree evaluation |
| `impl/web/faces` (top-level) | 445 | `FacesWebContext` constructor calls `getExternalContext().getRequest()` — NPE in mock; `FacesAction` execute path requires `FacesView` injection |
| `impl/web/faces/models` | 357 | `SkyveLazyDataModel` etc. need live query execution |
| `impl/web/faces/renderers` | 10 | Renderers |
| `impl/sail/execution` | 169 | SAIL execution — needs live browser |
| `impl/sail/execution/pf` | 1 102 | PrimeFaces SAIL — same |
| `impl/sail/execution/sc` | 529 | SmartClient SAIL — same |
| `impl/sail/interpret` | 33 | SAIL interpreter |
| `impl/sail/mock` | 230 | SAIL mocks — support code, not a test target |
| `impl/web/service/rest` | 208 | REST services — calls `CORE.*` without a bootstrappable path |
| `impl/web/filter/rest` | 304 | REST filter — same |

**Newly testable via MockFacesContext + H2 (removed from skip list; see Tier 3c below):**

| Package | Lines | Approach |
|---|---|---|
| `impl/web/faces/pipeline/component` | 3 953 | `AbstractSkyveTest` + `setSailFacesContextIfNeeded()` — exercise each widget-type branch |
| `impl/web/faces/pipeline` (layout + `FacesViewRenderer`) | 1 921 | Same — drive layout builder methods |
| `impl/web/faces/pipeline/layout` | 655 | Same |
| `impl/web/faces/charts/config` | 71 | Already using `new MockFacesContext()` directly — pure JUnit in `skyve-web` |

### Summary

| Module | Total lines | Skipped lines | Testable lines |
|---|---|---|---|
| `skyve-core` | 38 794 | ~11 900 | **~26 900** |
| `skyve-ext` | 20 694 | ~10 800 | **~9 900** |
| `skyve-web` | 26 368 | ~10 800 | **~15 600** |
| **Total** | **85 856** | **~33 500** | **~52 400** |

**Target: 100% of 52 400 testable lines.**  
**Current aggregate covered in testable packages: ~24 900 lines.**  
**Gap to close: ~27 500 additional lines.**

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
| `domain/types/converters/*` | 96–100% | Done — most converters at/near 100% |
| `domain/types/formatters` | 93.1% | Done |
| `impl/metadata/model` | 95.2% | Done |
| `impl/metadata/model/document/field` | 98.1% | Done |
| `impl/metadata/model/document/field/validator` | 94.1% | Done |
| `impl/metadata/module/menu` | 95.6% | Done |
| `impl/metadata/repository/behaviour` | 96.3% | Done |
| `impl/metadata/repository/view/actions` | 96.8% | Done |
| `metadata/repository` | 94.7% | Done |
| `cache` | 96.2% | Done |
| `impl/util/json` | 85.2% | Done |
| `metadata/view/model/map` | 100.0% | Done || `impl/metadata/repository/customer` | ~90% | convert() error branches done; 21 missed lines are in success path requiring H2 |
### Remaining work — below 80% in aggregate

| Package | Aggregate % | Lines remaining | Where | Difficulty |
|---|---|---|---|---|
| `impl/bind` | 61.0% | ~764 | `skyve-core` + `skyve-war` H2 | Medium — extend `BindUtilTest` |
| `util` | 61.7% | ~881 | `skyve-core` + `skyve-war` H2 | Medium — `Util`, `OWASP`, `Time` |
| `impl/util` | 68.1% | ~684 | `skyve-core` | Medium — string/date/crypto helpers |
| `util/test` | 75.9% | ~87 | `skyve-war` H2 | Low — `DataBuilder` edges |
| `impl/metadata/repository/document` | ~80% | ~96 | `skyve-war` H2 (success path only — error branches done) | Medium |
| `impl/metadata/repository/module` | ~73% | ~172 | `skyve-war` H2 (success path only — error branches done) | Medium |
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
| `job` (ext entry points) | 84.9% | Done |

### Remaining work

| Package | Aggregate % | Lines remaining | Base class | Difficulty |
|---|---|---|---|---|
| `impl/persistence/hibernate` | 23.0% | ~1 695 | `AbstractSkyveTest` | Hard — core Hibernate layer; highest single value |
| `impl/backup` | 6.1% | ~1 586 | `AbstractSkyveTest` | Hard — backup/restore |
| `impl/job` | 1.7% | ~653 | `AbstractSkyveTest` | Medium — job scheduling |
| `impl/script` | 58.5% | ~334 | `AbstractH2Test` | Medium — Groovy execution |
| `impl/archive/list` | 59.7% | ~129 | `skyve-ext` unit test | Low — mostly pure logic |
| `impl/archive/support` | 2.9% | ~202 | `skyve-ext` unit test | Medium |
| `impl/util` (ext) | 36.0% | ~409 | `skyve-ext` unit test | Medium |
| `impl/dataaccess/sql` | 27.9% | ~227 | `AbstractH2Test` | Medium |
| `util` (ext) | 28.7% | ~736 | mixed | Medium — `CommunicationUtil`, `Thumbnail` |
| `impl/security` | 14.3% | ~156 | mock or H2 | Medium |
| `impl/cache` (ext) | 4.2% | ~295 | `skyve-ext` unit test | Medium |
| `org.skyve` (ext facades) | 8.1% | ~226 | H2 or mock | Medium |

### Priority order

1. **`impl/archive/list`**: easiest remaining skyve-ext win, mostly pure logic.
2. **`impl/util`** (ext, 409 lines): utility helpers — many are pure Java.
3. **`impl/security`** (156 lines) and **`impl/cache`** (295 lines): medium complexity, mockable.
4. **`impl/persistence/hibernate`** (1 695 lines): the single highest-value package; already partially covered.
5. **`impl/backup`** (1 586 lines): high value but complex; needs multi-transaction patterns.
6. **`impl/job`** (653 lines): job scheduling — needs CDI + metadata.
7. **All remaining** — drive every testable package to 100%.

---

## Tier 3 — skyve-web: Converters (Pure Unit Tests) + H2-Backed Tests via `skyve-war`

`skyve-web` has two distinct testable surfaces.

### 3a — JSF Converter Layer (Pure Unit Tests in `skyve-web`)

Tests go in `skyve-web/src/test/java/`. No servlet, no CORE bootstrap needed.

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

**Target:** 100% of all testable converter lines (~651 remaining).

### 3b — H2-Backed Tests via `skyve-war`

`skyve-war` H2 tests that call `skyve-web` classes credit those lines to `skyve-web` in the aggregate report. This is the same mechanism that makes `skyve-war` H2 tests credit `skyve-core` and `skyve-ext`. The template is already established: `ViewJSONManipulatorTest` in `skyve-war/src/test/java/org/skyve/impl/web/service/smartclient/` exercises `skyve-web` service classes using `AbstractSkyveTest`, and those lines appear in the aggregate `skyve-web` report.

The testable `skyve-web` packages via this route are classes that call `CORE.*` or need a live `User`/`Customer`/`Document` graph but do **not** require a running JSF container or servlet dispatch:

| Package | Lines | Test base | Difficulty |
|---|---|---|---|
| `impl/web/service/smartclient` — manipulators, renderers, field/column/lookup definitions | ~1 200 | `AbstractSkyveTest` | Medium — `ViewJSONManipulatorTest` already started |
| `impl/web` — `WebUtil`, `AbstractWebContext`, `SkyveContextListener` | ~400 | `AbstractH2Test` | Medium |
| `impl/web/service` — `ClientDocument`, `UserSession` | ~300 | `AbstractH2Test` | Low-Medium |
| `impl/web/service/sse` — `SseClientHandler` | ~50 | `AbstractH2Test` + mock `SseEventSink` | Low |
| `impl/web/spring` — auth success/forward handlers | ~100 | `AbstractH2Test` | Low |
| `impl/web/filter` — `ResponseHeaderFilter`, `UTF8CharacterEncodingFilter`, `ExcludeStaticFilter` | ~270 | Plain Mockito (no CORE needed) | Low |
| `impl/web/filter/gzip` | ~246 | Plain Mockito + byte-array streams | Low-Medium |

Tests go in `skyve-war/src/test/java/org/skyve/impl/web/...` mirroring the `skyve-web` package layout.

### Packages with partial coverage (partially testable with partial Mockito approach)

| Package | % | Notes |
|---|---|---|
| `impl/web/spring` | 22.2% | Spring security config — `@WebMvcTest` style helps at the margins; main blocker is `CORE.*` calls |
| `impl/web/service/sse` | 79.4% | Nearly done — a few more SSE event paths via H2 test |

### 3c — JSF Pipeline/Component Builders via MockFacesContext + H2 (`skyve-war`)

The `impl/sail/mock` package ships a complete headless Faces mock (`MockFacesContext`, `MockApplication`, `MockELContext`, `MockExpressionFactory`) as production Skyve code. `FacesUtil.setSailFacesContextIfNeeded()` installs it on the current thread. `MockApplication` registers all PrimeFaces component types, so `createComponent()` works. `MockExpressionFactory` returns null for all EL expressions — components are created and widget-type routing is exercised, but expression evaluation is skipped.

SAIL already uses this to drive the pipeline; the 32.4% coverage on `impl/web/faces/pipeline/component` comes entirely from SAIL scripts. Direct tests bypass SAIL and call component builder methods one per widget type, which is far more systematic.

Tests go in `skyve-war/src/test/java/org/skyve/impl/web/faces/pipeline/` extending `AbstractSkyveTest`.

```java
@BeforeEach
void setUpFaces() {
    FacesUtil.setSailFacesContextIfNeeded();  // must come before any builder instantiation
}

@AfterEach
void tearDownFaces() {
    FacesUtil.resetSailFacesContextIfNeeded();
}
```

| Package | Lines | What to test |
|---|---|---|
| `impl/web/faces/pipeline/component` | 3 953 | One test per widget type in `ComponentBuilder` — verify the right PrimeFaces component type is created and key properties are set |
| `impl/web/faces/pipeline` (FacesViewRenderer, ResponsiveFormGrid) | ~650 | Pipeline orchestration logic |
| `impl/web/faces/pipeline/layout` | 655 | Layout builder methods |
| `impl/web/faces/charts/config` | 71 | Already using `new MockFacesContext()` directly — add pure JUnit tests in `skyve-web` |

### Priority order (Tier 3 combined)

1. **Converters (3a):** 651 lines, pure JUnit — complete the whole layer.
2. **Simple filters (3b):** `impl/web/filter` + `impl/web/filter/gzip` (~516 lines), plain Mockito.
3. **Chart config renderers (3c):** 71 lines, already use `new MockFacesContext()` — add tests in `skyve-web`.
4. **SmartClient layer (3b via H2):** extend `ViewJSONManipulatorTest` — ~1 200 lines.
5. **Component/layout builders (3c via H2 + mock):** systematic per-widget coverage — ~5 200 lines, highest single value in Tier 3.
6. **Web utilities + service + spring + SSE (3b):** `WebUtil`, `ClientDocument`, `UserSession`, auth handlers — ~850 lines.

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
| skyve-web converters (Tier 3a) | ~651 | Low |
| skyve-web simple filters (Tier 3b — plain Mockito) | ~516 | Low |
| skyve-web chart config renderers (Tier 3c) | ~71 | Low |
| skyve-web SmartClient layer (Tier 3b — `skyve-war` H2) | ~1 200 | Medium |
| skyve-web component/layout builders (Tier 3c — H2 + MockFacesContext) | ~5 200 | Medium |
| skyve-web utilities + service + spring + SSE (Tier 3b — `skyve-war` H2) | ~850 | Low-Medium |
| **Total gap to 100%** | **~27 500** | |

---

## Working Order

1. **Quick wins first:** finish all small-gap packages completely (< 50 lines each). Get them to 100%.
2. **skyve-web converters (Tier 3a):** 651 lines, all pure JUnit, highly mechanical — complete the whole layer.
3. **skyve-web simple filters (Tier 3b):** `impl/web/filter` and `impl/web/filter/gzip` (~516 lines), plain Mockito, no CORE dependency.
4. **skyve-web chart config renderers (Tier 3c):** 71 lines, already pattern-matched with `new MockFacesContext()` — add tests in `skyve-web`.
5. **Core utilities:** `impl/bind`, `util`, `impl/util` — high traffic code, high value for regression safety. Drive to 100%.
6. **Metadata resolution:** `impl/metadata/user`, `impl/metadata/customer`, `impl/metadata/view/component` — requires H2 but well-bounded.
7. **Chart/comparison models:** mostly pure Java metadata types in `skyve-core`; map models are complete.
8. **skyve-web SmartClient layer (Tier 3b):** extend `ViewJSONManipulatorTest` — manipulators, renderers, field/column/lookup definitions via `AbstractSkyveTest` in `skyve-war`.
9. **skyve-web component/layout builders (Tier 3c):** `AbstractSkyveTest` + `FacesUtil.setSailFacesContextIfNeeded()` — one test per widget type in `ComponentBuilder`; highest single Tier 3 value at ~5 200 lines.
10. **skyve-web utilities + service + spring + SSE (Tier 3b):** `WebUtil`, `ClientDocument`, `UserSession`, auth handlers — via `AbstractH2Test` in `skyve-war`.
11. **skyve-ext easy tier:** archive/list, security, cache, ext utilities — many are mockable; ext `job` entry points are now above target.
12. **skyve-ext heavy tier:** hibernate persistence layer, backup/restore, job scheduling — hardest but highest absolute line count.
13. **Admin module domain tests** (Tier 4) — after framework packages are complete.

---

## Recommended Long-Run Chunks

These are the best larger chunks for longer autonomous runs. They are grouped to keep setup coherent, minimise context switching, and produce meaningful report movement per session.

### 1. Core binding and expression stack

- Scope: `org.skyve.impl.bind`, plus adjacent high-touch classes in `org.skyve.util` and `org.skyve.impl.util` that support expression and binding paths.
- Current measured misses:
    - `org.skyve.impl.bind`: 1 049 missed / 909 covered in the current `skyve-core` report.
    - `BindUtil`: 711 missed.
    - `ELExpressionEvaluator`: 183 missed.
    - `ValidationELResolver`: 66 missed.
    - `MetaDataExpressionEvaluator`: 60 missed.
    - `DataBuilder`: 72 missed.
    - `Binder`: 19 missed.
    - `ExpressionEvaluator`: 56 missed.
- Why it clusters well: these classes share the same expression resolution, binding, validation, and formatter pathways, so the same fixtures and test idioms pay off across multiple classes.
- Run length: long. This is one of the best multi-session chunks in the repository.

### 2. Core utility infrastructure

- Scope: `org.skyve.impl.util` and the remaining hard edges in `org.skyve.util`.
- Current measured misses:
    - `org.skyve.impl.util`: 781 missed / 723 covered.
    - `XMLMetaData`: 325 missed.
    - `UtilImpl`: 207 missed.
    - `ValidationUtil`: 198 missed.
    - `BeanValidator`: 51 missed.
    - `RuntimeCompiler`: 43 missed.
- Why it clusters well: these are reusable helpers with broad call surfaces. Even when individual classes are awkward, the test style is consistent: pure unit tests, parser-style cases, null/error branches, and structured input/output assertions.
- Run length: long.

### 3. Core metadata runtime and H2-backed resolution

- Scope: `impl/metadata/view`, `impl/metadata/view/component`, `impl/metadata/repository/module`, `impl/metadata/user`, `impl/metadata/customer`, plus adjacent H2-backed metadata resolution paths.
- Current measured or planned misses:
    - `org.skyve.impl.metadata.view`: 917 missed / 159 covered in the current `skyve-core` report.
    - `org.skyve.impl.metadata.repository.module`: 576 missed / 364 covered.
    - `org.skyve.impl.metadata.user`: 391 missed / 153 covered.
    - `impl/metadata/view/component`: ~183 remaining in the aggregate plan.
    - `impl/metadata/customer`: ~199 remaining in the aggregate plan.
- Why it clusters well: these classes need the same H2-backed scaffolding and metadata graph setup. Once that harness is hot, multiple packages can be advanced in the same longer run.
- Run length: long, with `skyve-war` test cycles.

### 4. skyve-ext scheduler and job runtime

- Scope: `org.skyve.impl.job` and the remaining uncovered paths around `org.skyve.job`.
- Current measured misses:
    - `org.skyve.impl.job`: 647 missed / 17 covered.
    - `QuartzJobScheduler`: 392 missed.
    - `ContentGarbageCollectionJob`: 108 missed.
    - `AbstractSkyveJob`: 87 missed.
    - `org.skyve.job` is already above target at 84.9%, but `ViewBackgroundTask` still has meaningful remaining surface.
- Why it clusters well: these classes share scheduler setup, job context, Quartz integration, and transaction/job lifecycle behavior. One longer run can stay entirely inside job orchestration concerns.
- Run length: long, likely mixed mock plus H2 work.

### 5. skyve-ext archive and export utility cluster

- Scope: `org.skyve.impl.archive.list` plus tractable utility classes in `org.skyve.impl.util`.
- Current measured misses:
    - `ArchivedDocumentListModel`: 75 missed.
    - `LuceneResultsIterable`: 34 missed.
    - `LuceneResultsIterator`: 9 missed.
    - `ImageUtil`: 96 missed.
    - `WebStatsUtil`: 80 missed.
    - `ExportedReferenceVisitor`: 63 missed.
    - `ExportedReferenceVisitor.Dereferencer`: 64 missed.
- Why it clusters well: this is a good “medium-large” ext unit-test run that stays mostly out of container-heavy scheduling and backup code while still covering real framework behavior.
- Run length: medium to long.

### 6. skyve-web converter and filter layer as one mechanical sweep

- Scope: all testable JSF converters under `org.skyve.impl.web.faces.converters.*` (Tier 3a), plus the simple servlet filters `impl/web/filter` and `impl/web/filter/gzip` (Tier 3b — plain Mockito, no CORE dependency).
- Current measured misses:
    - `SelectItemsIterator`: 59 missed.
    - `GenericObjectSelectItem`: 41 missed.
    - `SelectItemsBeanConverter`: 30 missed.
    - `AssociationAutoCompleteConverter`: 19 missed.
    - `AssociationPickListConverter`: 17 missed.
    - Many decimal and currency converters are still at 13 missed each.
    - `GeometryConverter`: 13 missed.
    - `ResponseHeaderFilter`, `UTF8CharacterEncodingFilter`, `ExcludeStaticFilter`, GZIP filter (~516 lines combined).
- Why it clusters well: converters are repetitive and low-context; filters need only Mockito mocks of `HttpServletRequest`/`HttpServletResponse`/`FilterConfig`. Once the first few tests are patterned, the rest is mechanical and suitable for a sustained longer run.
- Run length: medium, but highly productive.

### 6b. skyve-web SmartClient layer via `skyve-war` H2

- Scope: `impl/web/service/smartclient` — manipulators, renderers, and field/column/lookup definition classes beyond `ViewJSONManipulator`.
- Template: `ViewJSONManipulatorTest` in `skyve-war/src/test/java/org/skyve/impl/web/service/smartclient/` already works. Tests go alongside it, extending `AbstractSkyveTest`.
- Current measured misses: ~1 200 lines in the package.
- Why it clusters well: all classes share the same session setup; the test template is proven.
- Run length: medium to long.

### 6c. skyve-web component builder pipeline via MockFacesContext + H2

- Scope: `impl/web/faces/pipeline/component` (3 953 lines), `impl/web/faces/pipeline` (pipeline orchestration + `FacesViewRenderer`), `impl/web/faces/pipeline/layout` (655 lines).
- Approach: `AbstractSkyveTest` (H2 session) + `FacesUtil.setSailFacesContextIfNeeded()` in `@BeforeEach`. Instantiate `ResponsiveComponentBuilder` or `DeviceResponsiveComponentBuilder` and call each widget-creation method with a live `Document`/`Bean`. Assert the returned `UIComponent` type and key non-null properties.
- Why it clusters well: all 70+ widget methods in `ComponentBuilder` follow the same pattern. The test harness is set up once; each widget type is a single small test method.
- Key constraint: `MockExpressionFactory` returns null for all EL expressions. Component instances are created and widget routing is fully exercised, but value expression evaluation is not. Tests must assert component type and static properties, not evaluated EL values.
- Run length: long. This is the single highest-value Tier 3 chunk (~5 200 lines).

### 7. skyve-ext heavy infrastructure tranche

- Scope: `impl/persistence/hibernate`, `impl/backup`, and selected non-skipped parts of `impl/job` once the scheduler harness is in place.
- Current measured misses:
    - `org.skyve.impl.persistence.hibernate`: 1 695 missed / 506 covered.
    - `org.skyve.impl.backup`: 1 586 missed / 103 covered.
    - `org.skyve.impl.job`: 647 missed / 17 covered.
- Why it clusters well: these are the highest-value long-haul packages in `skyve-ext`, but they only make sense once a reliable persistence/scheduler harness is already working. This is the right place for deliberately longer, fewer-context-switch sessions.
- Run length: very long.

## Best Next Long Runs

If the goal is fewer but bigger coverage pushes, the highest-yield order is:

1. **Core binding and expression stack**
2. **Core utility infrastructure**
3. **Core metadata runtime and H2-backed resolution**
4. **skyve-ext scheduler and job runtime**
5. **skyve-web converter layer**

This order favors large testable surfaces that are not on the skip list, reuse the same setup per session, and avoid spending early time in the heaviest persistence and backup infrastructure before the easier large chunks are exhausted.

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
