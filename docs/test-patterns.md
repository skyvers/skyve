# Test Patterns Cookbook

`docs/test-patterns.md` captures reusable test patterns seen across Skyve repositories, generalised for framework and application work.

Use the smallest pattern that proves the behaviour. For the active coverage improvement plan, target packages, and skip list, see [docs/coverage-plan.md](coverage-plan.md).

## Test Placement Decision

Pick the narrowest base class that makes the test compile and run correctly. The hierarchy in `skyve-war` is:

```
InternalBaseH2Test
  └── AbstractH2Test           (JUnit 5 lifecycle hooks, plain H2 session)
        ├── AbstractH2TestTruncate  (truncates all rows after each test — use for multi-commit tests)
        │     └── AbstractSkyveTest       (adds pre-resolved Customer/Module/Document handles)
        │           └── AbstractSkyveTestDispose  (dispose+recreate Persistence after each test)
        ├── AbstractH2TestDispose        (dispose strategy without skyve document handles)
        └── AbstractDomainTest<T>        (generic CRUD contract tests via abstract getBean())
```

Quick decision rules:

| Scenario | Base class | Module |
|---|---|---|
| Pure Java — type system, converters, formatters, fluent builders, utilities | plain JUnit 5 | `skyve-core` |
| Mockito — stub collaborators at a narrow seam | `@ExtendWith(MockitoExtension.class)` | `skyve-core` |
| Needs `CORE.getPersistence()`, BizQL, or Bizlet callbacks | `AbstractSkyveTest` | `skyve-war` |
| Needs a live session but not a full document graph | `AbstractH2Test` | `skyve-war` |
| Testing a specific document's CRUD contract | `AbstractDomainTest<MyDoc>` | `skyve-war` |
| JUnit 4 test needing H2 (legacy — do not create new) | `AbstractH2TestForJUnit4` | `skyve-war` |

### Import paths for test base classes

The base classes live in two packages inside `skyve-war/src/test/java/`. Getting the import wrong is a common first-attempt failure.

```java
// H2 infrastructure — package util
import util.AbstractH2Test;
import util.AbstractH2TestTruncate;
import util.AbstractH2TestDispose;
import util.AbstractDomainTest;
import util.AbstractH2TestForJUnit4;   // JUnit 4 only — legacy

// Full Skyve document handles — package modules.test
import modules.test.AbstractSkyveTest;          // truncate strategy
import modules.test.AbstractSkyveTestDispose;   // dispose strategy
```

Tests written in `skyve-war` that call `skyve-core` classes **count toward `skyve-core` coverage** in the JaCoCo aggregate report — the `skyve-war` module is a `classifier=classes` dependency of `skyve-coverage`. Prefer writing coverage tests in `skyve-core` when no H2 is needed; fall back to `skyve-war` when the code under test demands a live session.

**Naming convention:** `*H2Test.java` for anything that touches H2; `*Test.java` for pure unit / Mockito tests.

### Eclipse JDT warning suppression

The project commits `.settings/org.eclipse.jdt.core.prefs` files that configure Eclipse JDT warning levels. VS Code surfaces these via `get_errors`. In test code, the most common warning is "method can be declared static" on `@Test` methods. Suppress it at the method or class level:

```java
@SuppressWarnings("static-method")
@Test
void roundTripsDateValue() {
    // ...
}
```

Prefer method-level suppression when only a few methods trigger the warning. Use class-level `@SuppressWarnings("static-method")` when the majority of test methods in the class are flagged.

### Test resources on the classpath (`skyve-core`)

`skyve-core/src/test/resources/` contains:

| Path | Contents |
|---|---|
| `json/*.json` | `skyve.json` (config), `withComments.json`, `withoutComments.json`, `blockComments.json` — used by `UtilImplTest` |
| `schemas/*.xsd` | Metadata XSDs (document, module, view, customer, router, behaviour, sail, common) |
| `resources/i18n.properties` | Internationalisation test strings |
| `org/skyve/impl/metadata/repository/router/router.xml` | Sample router metadata |

## Pure Mockito Unit Tests

- Purpose: Fast branch and exception coverage of one class by stubbing collaborators.
- Tooling: `@Mock`, `@Spy`, `@InjectMocks`, and static mocking only when unavoidable.
- Guidance: Prefer constructor or field injection seams over reflection; use reflection only when no cleaner seam exists.

```java
@ExtendWith(MockitoExtension.class)
class CalculatorTest {
  @Mock Dependency dep;
  @Spy @InjectMocks Calculator calculator;

  @Test
  void failsOnClosedAccount() {
    when(dep.getStatus()).thenReturn(Status.CLOSED);
    assertThrows(DomainException.class, () -> calculator.compute(input));
    verify(dep, never()).persist(any());
  }
}
```

## Validation-Focused Tests (No DB)

- Purpose: Cover `preExecute()`/`validate()` rules and message text without persistence.
- Tooling: Mockito only; assert `ValidationException` content and message count.

```java
@ExtendWith(MockitoExtension.class)
class ValidatorTest {
  @Mock Bean bean;
  @Spy @InjectMocks Validator validator;

  @Test
  void nextDateAfterFinalDateRaisesMessage() {
    when(bean.getNextDate()).thenReturn(tomorrow());
    when(bean.getFinalDate()).thenReturn(today());

    ValidationException ex = assertThrows(
        ValidationException.class,
        () -> validator.preExecute(ImplicitActionName.Save, bean, null, null));

    assertThat(ex.getMessages(), hasSize(1));
  }
}
```

## Data-Backed Service Tests (H2)

- Purpose: Exercise repository/service behaviour with realistic persisted data.
- Tooling: `AbstractH2Test` (or module base), generated `newInstance()` and `DataBuilder` fixtures.
- Guidance: Assert before/after counts and key field changes.

```java
class PaymentServiceH2Test extends AbstractH2Test {
  @Inject PaymentService service;

  @Test
  void updateMovesForeignKey() {
    Claim oldClaim = save(Claim.newInstance());
    Claim newClaim = save(Claim.newInstance());
    Payment payment = save(Payment.newInstance());
    payment.setClaim(oldClaim);

    int moved = service.updatePaymentClaimId(oldClaim.getBizId(), newClaim.getBizId());

    assertThat(moved, is(1));
    assertThat(retrieve(payment).getClaim(), is(newClaim));
  }
}
```

## Search and ViewModel Filtering (H2)

- Purpose: Verify list/search models return expected rows with and without filters.
- Tooling: H2 base, seeded fixtures, model `postConstruct(getCustomer(), true)`.
- Guidance: Prefer asserting stable projected values rather than object identity.

## Job or Action Orchestration with Partial Mocking

- Purpose: Cover batch/job/action flows while stubbing heavy seams (reporting, filesystem, remote systems).
- Tooling: H2 base for persistence side effects + Mockito for external seams.
- Guidance: Verify both persistence outcomes and collaborator interactions.

## Bizlet Transformation and Validation Tests

- Purpose: Cover small `preSave()` mutation rules and `validate()` message behaviour.
- Tooling: Direct Bizlet instance + bean spies/mocks.
- Guidance: Keep tests focused on one rule at a time.

## Utility and Formatting Tests

- Purpose: Deterministic helpers (string/date/formatting math).
- Tooling: Plain JUnit only.
- Guidance: No fixtures, no database, no mocks unless absolutely required.

## Hybrid H2 + Mock Seams

- Purpose: Exercise realistic persistence state while mocking infrastructure seams.
- Tooling: H2 test base + `@Mock` for persistence wrappers/reporting APIs.
- Guidance: Use when pure unit tests are too synthetic and full integration is too heavy.

## skyve-war as a Full-Stack Test Harness

`skyve-war` is the assembled reference application and the only module that runs with the full Skyve metadata context: real customer metadata, the complete test module domain, a real `SessionFactory`, and a `SuperUser` with access to all documents. Tests that need any of those things belong here.

### What the test module provides

The `test` module lives in `skyve-war/src/main/java/modules/test/` and is specifically designed to stress-test every framework capability:

| Category | Document(s) |
|---|---|
| All scalar attribute types | `AllAttributesPersistent`, `AllAttributesRequiredPersistent` |
| All dynamic attribute types | `AllDynamicAttributesPersistent`, `AllAttributesDynamicPersistent` |
| Embedded associations | `AllAttributesEmbedded` |
| Single-table and joined inheritance (extension) | `MappedExtensionSingleStrategy`, `MappedExtensionJoinedStrategy` and their unique variants |
| Single-table and joined inheritance (subclassing) | `MappedSubclassedSingleStrategy`, `MappedSubclassedJoinedStrategy` and their unique variants |
| Polymorphic arc associations | `ArcOneToOne`, `ArcOneToMany`, `AnyBase`, `AnyDerived1`, `AnyDerived2` |
| Inverse collections | `InverseOneToManyPersistent`, `InverseOneToOnePersistent`, `InverseManyToManyPersistent` |
| Unique constraints (nullable, non-nullable, multiple navigable, optimisation) | `UniqueConstraint*` |
| Hierarchical (self-referential) | `Hierarchical` |
| Reachability (cascade delete) | `Reachability` |
| Dynamic persistence | `DynamicMappedExtension`, `DynamicMappedSubclassed` |

`AllAttributesPersistent` is the most commonly used fixture document. It has every scalar attribute type (bool, colour, date, dateTime, decimal2/5/10, enum, geometry, id, integer, longInteger, markup, memo, text, time, timestamp), an aggregated association, a composed association, an embedded association, and both aggregated and composed collections.

### The test base-class hierarchy

```
InternalBaseH2Test          (boots H2, Weld CDI, customer "bizhub", schema, SuperUser)
  └── AbstractH2Test        (JUnit 5 lifecycle hooks: setUp/tearDown/beforeBase/afterBase)
        ├── AbstractH2TestTruncate   (@AfterEach: rollback + truncate all tables + new transaction)
        │     └── AbstractSkyveTest         (resolves u, c, m, aapd, … document refs per test)
        └── AbstractH2TestDispose    (@AfterEach: rollback + disposeAllPersistenceInstances)
              └── AbstractSkyveTestDispose  (same document refs, dispose strategy)
```

**Choose `AbstractSkyveTest`** (truncate strategy) when the code under test performs its own commits (e.g. jobs, backup/restore, multi-transaction orchestration). The truncate step purges all rows after each test so the next test starts clean.

**Choose `AbstractSkyveTestDispose`** (dispose strategy) when the test performs a single transaction and a rollback is sufficient for cleanup. Dispose tears down and recreates the `Persistence` instance, which is heavier but simpler in reasoning.

Each test method receives pre-resolved document handles (`aapd` for `AllAttributesPersistent`, `aarpd` for `AllAttributesRequiredPersistent`, etc.) and a live `Persistence p`, `User u`, `Customer c`, and `Module m`.

### When to write a test here (vs skyve-ext or pure unit)

Write a test in `skyve-war` when the code under test:

- **Resolves `{module.Document}` syntax** in a BizQL or named query — requires the metadata repository.
- **Uses `AbstractPersistence.get()`** in its own implementation — requires a real `ThreadLocal`-bound persistence instance with a user.
- **Exercises Bizlet callbacks** (`postLoad`, `preSave`, `postRemove`) fired by Hibernate event listeners — requires a full `HibernateListener` wired to a session that has a bound `AbstractPersistence`.
- **Tests cascade-delete, referential-integrity enforcement, or unique-constraint checking** end-to-end through the persistence lifecycle.
- **Uses `Util.constructRandomInstance(u, m, doc, depth)`** to generate realistic fixture graphs with valid associations and collections at a chosen depth.
- **Exercises query factory methods** that depend on metadata lookup: `newNamedBizQL`, `newNamedDocumentQuery`, `newDocumentQuery(Bean)`.
- **Tests the DDL generation path** (`generateDDL`) against the full entity set.

### Typical test shape

```java
public class MyTests extends AbstractSkyveTestDispose {

    @Test
    public void savesAndRetrievesBeanWithAllAttributes() throws Exception {
        // arrange: random valid instance at depth 1 (no sub-graph)
        AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 1);

        // act
        AllAttributesPersistent saved = p.save(bean);
        AllAttributesPersistent retrieved = p.retrieve(
                AllAttributesPersistent.MODULE_NAME,
                AllAttributesPersistent.DOCUMENT_NAME,
                saved.getBizId());

        // assert
        Assert.assertNotNull(retrieved);
        Assert.assertEquals(saved.getBizId(), retrieved.getBizId());
    }

    @Test
    public void bizQLReturnsOnlySavedBean() throws Exception {
        AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
        p.save(saved);

        List<AllAttributesPersistent> results = p.newBizQL(
                "select bean from {test.AllAttributesPersistent} as bean" +
                " where bean.bizId = :id")
                .putParameter("id", saved.getBizId())
                .beanResults();

        Assert.assertEquals(1, results.size());
        Assert.assertEquals(saved.getBizId(), results.get(0).getBizId());
    }
}
```

### `Util.constructRandomInstance` depth parameter

The depth parameter controls sub-graph construction:

- `depth=1` — scalar attributes populated, no nested associations or collections.
- `depth=2` — one level of associations and collections populated.
- `depth=N` — recursive up to N levels.

Use the minimum depth that satisfies the test. Deeper graphs are heavier to save and generate more Hibernate INSERT statements.

---

## Hibernate Persistence Tests: When to Mock vs When to Use H2

This is the most common judgement call when testing `skyve-ext` persistence code.

### Use a mock Session when:

- Testing **error paths and exception-type mapping** (e.g. `QueryTimeoutException` → `TimeoutException`, `ObjectNotFoundException` → null return). The real database rarely throws these naturally in unit tests.
- Testing **parameter-dispatch branches** inside `createQueryFromSQL()` or `createHibernateQuery()`. These are long `if/else if` chains over value types and `AttributeType` — you need many small inputs to cover every arm, not a real query. Inject a mock `NativeQuery` so `setParameter()`/`setParameterList()` calls can be verified or silently absorbed:

```java
Session session = mock(Session.class);
injectField(AbstractHibernatePersistence.class, p, "session", session);
NativeQuery<?> q = mock(NativeQuery.class);
when(session.createNativeQuery(anyString())).thenReturn(q);
when(q.list()).thenReturn(List.of());
// now set the typed parameter and call scalarResults() — the dispatch branch fires
sql.putParameter("t", (Object) new TimeOnly(), AttributeType.time);
sql.scalarResults(Integer.class);
```

- Testing **fluent API contracts** (`putParameter` returns `this`, `setFirstResult` returns `this`) — no database needed.
- Testing **constructor error guards** (e.g. calling `beanResults()` without setting module/document throws `DomainException`).
- Testing **`HibernateQueryDelegate` validation** (`assertSingle`, `assertMultiple` alias-count checks).

### Use a real H2 session factory when:

- Testing **actual query execution** (all `beanResults`, `tupleResults`, `scalarResults`, `projectedResults`, and their iterable counterparts) — only real execution proves the generated HQL or native SQL is valid.
- Testing **DDL generation** (`generateDDL`) — H2 must exist to produce output.
- Testing **save/delete/merge lifecycle** — covers `HibernateListener` event callbacks, optimistic-lock stamping, cascade rules, and unique-constraint checking.
- Testing **scroll cursors** (`beanIterable`, `tupleIterable`, `scalarIterable`) — requires a live `ScrollableResults` from a real session.
- Testing **`DocumentQuery` with filter/from/group/order clauses** — the HQL assembler combines these into a real string that must parse.

### Use the hybrid pattern (bootstrapped H2 + injected mock Session) when:

- You need a real **session factory** (so entity metadata and dialect are available) but want to intercept the actual **JDBC call** to control what is returned or thrown. Inject the mock after `begin()` has established the session:

```java
AbstractHibernatePersistenceTest.TestHibernatePersistence p = new TestHibernatePersistence();
// session factory bootstrapped by @BeforeAll
// inject mock session to intercept without executing SQL:
Field f = AbstractHibernatePersistence.class.getDeclaredField("session");
f.setAccessible(true);
f.set(p, mockSession);
```

### Use `skyve-war` PersistenceTests (full-stack H2) when:

- The test requires a **real Skyve metadata context** — customer, module, document, user — because the code under test calls `AbstractPersistence.get()`, resolves document entity names from module metadata, or fires Bizlet callbacks.
- Testing **named query resolution** (`newNamedBizQL`, `newNamedDocumentQuery`).
- Testing **query-by-example** (`newDocumentQuery(Bean)`).
- Testing **BizQL with `{module.Document}` syntax** (requires the metadata repository to resolve entity names).

---

## Hibernate Persistence Test Infrastructure Patterns

### Bootstrapping a minimal H2 session factory in skyve-ext

A `@BeforeAll` method calls `AbstractHibernatePersistenceTest.setupPersistenceBootstrap()`, which:
1. Configures `UtilImpl.DATA_STORE` pointing at an in-memory H2 database.
2. Registers only the minimal entities needed (e.g. `adminContact`) rather than the full schema.
3. Builds a Hibernate `SessionFactory` that persists for the life of the test class.

Each test creates a `TestHibernatePersistence` (an inner class with no-op content methods), calls `begin()`, does its work, then `rollback()` / `close()` in a `finally` block.

### Thread-local binding for `AbstractPersistence.get()`

`HibernateDocumentQuery` calls `AbstractPersistence.get()` during construction to read user context. Bind the test instance to the current thread before constructing the query, and unbind in `finally`:

```java
AbstractHibernatePersistenceTest.bindPersistenceToThread(p);   // ThreadLocal injection via reflection
try {
    HibernateDocumentQuery dq = new HibernateDocumentQuery(doc, p);
    // ...
} finally {
    AbstractHibernatePersistenceTest.unbindPersistenceFromThread();
}
```

### Pre-resolving HQL to bypass metadata lookup in BizQL tests

`HibernateBizQL.toQueryString()` resolves `{module.Document}` placeholders using the metadata repository. In unit tests without the full repository, inject the resolved HQL directly via reflection:

```java
Field rq = AbstractBizQL.class.getDeclaredField("resolvedQuery");
rq.setAccessible(true);
rq.set(bizQLInstance, "select bean as bean from adminContact as bean");
```

Always use explicit `AS alias` clauses — `getReturnAliases()` returns `null` for bare entity selects, causing a `NullPointerException` in the `TreeMap`-backed result assembly.

### Covering collection, array, and scalar parameter-dispatch arms

The `createQueryFromSQL()` parameter dispatch has three arms per `AttributeType`: `Collection<?>`, `Object[]` array, and scalar. Cover all three with separate tests, passing the same `AttributeType` each time:

```java
// Collection arm
sql.putParameter("ids", (Object) List.of("a", "b"), AttributeType.id);
// Array arm
sql.putParameter("ids", (Object) new String[] {"a", "b"}, AttributeType.id);
// Scalar arm
sql.putParameter("id", (Object) "abc", AttributeType.id);
```

The early-exit branches (before the `AttributeType` switch) require passing the specific Skyve type as a raw `Object` with an explicit `AttributeType` so the instance-check fires but the parameter still reaches the dispatch:

```java
// Forces the TimeOnly early-exit (sets TimeType.INSTANCE)
sql.putParameter("t", new TimeOnly());
// Forces the OptimisticLock early-exit (sets StringType.INSTANCE)
sql.putParameter("lock", (Object) new OptimisticLock("user", new Date()), AttributeType.text);
```

---

## JaCoCo Aggregate Coverage: Ensuring All Modules Contribute

The `skyve-coverage` aggregate module collects `.exec` files only from Maven dependencies. A module that builds and runs tests (like `skyve-war`) but is not listed as a dependency of `skyve-coverage` contributes **zero coverage** to the aggregate report, even though its tests exercise the framework code.

To include a WAR module's test execution in the aggregate, add it with the `classes` classifier:

```xml
<dependency>
    <groupId>org.skyve</groupId>
    <artifactId>skyve-war</artifactId>
    <version>${project.version}</version>
    <classifier>classes</classifier>
</dependency>
```

This requires `skyve-war/pom.xml` to already set `<attachClasses>true</attachClasses>` in the WAR plugin configuration (which it does).

---

## Skyve-Specific Test Guidance

- Naming split is intentional:
  - `*H2Test.java` for persistence/integration behaviour.
  - `*Test.java` for pure unit/Mockito behaviour.
- Prefer generated `DocumentName.newInstance()` over constructors for real document instances.
- Prefer `DataBuilder` when surrounding tests already use it for valid graph setup.
- For metadata or generation changes, run generation before trusting test results.
- For access-control or router changes, include manual verification steps when automated coverage cannot prove UI navigation constraints.
- For actions with web context dependencies, cover both web and non-web code paths.
- `Bean.DOCUMENT_ID` is the constant for `"bizId"`; `Bean.BIZ_KEY` is the constant for `"bizKey"`. There are no `BIZ_ID_ATTRIBUTE_NAME` or `BIZ_KEY_ATTRIBUTE_NAME` constants.
- When testing `HibernateQueryDelegate.list()` in projected (non-asIs) mode, HQL must include explicit `AS alias` clauses. A bare entity-select (`select bean from ...`) returns `getReturnAliases() == null`, which causes a `NullPointerException` when populating the `TreeMap`-backed `DynamicBean`.

---

## Code Coverage and JaCoCo Profile

Skyve uses JaCoCo for code coverage analysis, but coverage is only collected when the special Maven profile `coverage` is enabled. This keeps normal builds and CI runs fast.

- **Normal builds** (without `-Pcoverage`) do not run JaCoCo, so they are faster and do not generate coverage data or reports.
- **Coverage builds** (with `-Pcoverage`) enable JaCoCo and generate per-module and aggregate coverage reports.

To run a coverage build and generate reports:

    mvn -Pcoverage -pl skyve-coverage -am -DskipIntegrationTests=true -DskipUnitTests=false verify

- The `skyve-coverage` module aggregates coverage from all main modules.
- Coverage reports are generated in each module and aggregated in `skyve-coverage/target/site/jacoco-aggregate/`.
- This approach is opt-in: only use coverage builds when you need to measure or improve test coverage.

For more on test types and patterns, see the sections above. For coverage troubleshooting or advanced usage, see the agent documentation in `agents.md`.
