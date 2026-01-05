Test Patterns Cookbook
======================

This document outlines reusable JUnit patterns seen across Skyve projects, expressed without module-specific context. Pick the style that matches what you need to prove.

Test Naming Conventions
-----------------------
Skyve tests should follow consistent naming conventions to clearly indicate their type and purpose:

- **Mock tests** (pure Mockito unit tests): Use the name of the class being tested and end with `Test`, e.g. `TagTest`
- **H2 tests** (tests extending `AbstractH2Test` or similar H2 test bases): Include `H2` in the name, e.g. `TagH2Test`
- **Integration tests**: End with `IT`, e.g. `TagIT.java`

Examples:
- `TagTest` - Mockito-only unit test for the `Tag` class
- `TagH2Test` - H2 persistence test for the `Tag` class
- `TagIT` - Integration test for the `Tag` class

Fixtures and DataBuilder
------------------------
Skyve tests should use `DataBuilder` to construct domain objects from metadata instead
of hand-building fixtures in each test.

### Rules
- **Never use `new` to instantiate a Skyve document.** Always call `MyDocument.newInstance()` when you need a real instance.
- **H2 tests** must use `DataBuilder` or factory fixtures for all domain object creation.
- **Mockito-only tests** must mock Skyve documents; avoid calling `newInstance()` unless strictly required.
- Use `db.build(<MODULE>, <DOC>)` for general objects, and `db.factoryBuild(...)` for complex or linked fixtures.

### Minimal example
```java
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

class ExampleH2Test extends AbstractH2Test {
  DataBuilder db = new DataBuilder().fixture(FixtureType.crud);

  @Test
  void createsValidDomainInstances() {
    Account acc = CORE.getPersistence().save(db.build(Account.MODULE_NAME, Account.DOCUMENT_NAME));
    DocumentQuery q = CORE.getPersistence().newDocumentQuery(Account.MODULE_NAME, Account.DOCUMENT_NAME);
    q.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
    assertThat(q.scalarResult(Number.class).intValue(), is(1));
  }
}
```

- **H2 / persistence tests** (`AbstractH2Test`, `AbstractDomainTest`, etc.):
  - Create a shared builder:
    ```java
    DataBuilder db = new DataBuilder().fixture(FixtureType.crud);
    ```
  - Build instances using module/document names:
    ```java
    Account account = CORE.getPersistence().save(db.build(Account.MODULE_NAME, Account.DOCUMENT_NAME));
    ```
  - This ensures values comply with masks, validators and relationships without
    needing custom private helper methods everywhere.

- **Factories and special fixtures**:
  - When random data is not sufficient, use a `<DocumentName>Factory` with
    `@SkyveFixture` methods which internally use `DataBuilder` or `factoryBuild(...)`
    to construct the base instance.

- **Mockito-only unit tests**:
  - Do **not** use `DataBuilder` or real persistence.
  - Mock Skyve documents instead of constructing them; if a concrete instance is
    unavoidable, use `MyDoc.newInstance()` (never `new MyDoc()`).

- **Global rule**: never `new` a Skyve document; use `newInstance()` or
  `DataBuilder`/factory methods.

Pure Mockito unit tests
-----------------------
- Purpose: fast branch/exception coverage of a class by stubbing every collaborator.
- Tooling: `@Mock`, `@Spy`, `@InjectMocks`, static mocking for factories/singletons.
- Tips: inject tricky private fields via reflection only when necessary; swallow logging/persistence in `doAnswer`.
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

Validation-focused tests (no DB)
--------------------------------
- Purpose: cover `preExecute`/`validate` rules and message text without hitting persistence.
- Tooling: Mockito only; assert on `ValidationException` contents.
```java
@ExtendWith(MockitoExtension.class)
class ValidatorTest {
  @Mock Bean bean;
  @Spy @InjectMocks Validator validator;

  @Test
  void nextDateAfterFinalDateRaisesMessage() {
    when(bean.getNextDate()).thenReturn(tomorrow());
    when(bean.getFinalDate()).thenReturn(today());
    var ex = assertThrows(ValidationException.class,
        () -> validator.preExecute(ImplicitActionName.Save, bean, null, null));
    assertThat(ex.getMessages()).hasSize(1);
  }
}
```

Builder/persistence sanity checks
---------------------------------
- Purpose: ensure builders populate defaults and write/read correctly.
- Tooling: lightweight H2 test base + `DataBuilder`; assert record counts before/after.
```java
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

class BuilderIntegrationTest extends AbstractH2Test {
  DataBuilder db = new DataBuilder().fixture(FixtureType.crud);

  @Test
  void builderUpserts() {
    DomainObject owner = CORE.getPersistence().save(db.build(DomainObject.MODULE_NAME, DomainObject.DOCUMENT_NAME));
    
    DocumentQuery q1 = CORE.getPersistence().newDocumentQuery(DomainObject.MODULE_NAME, DomainObject.DOCUMENT_NAME);
    q1.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
    assertThat(q1.scalarResult(Number.class).intValue(), is(1));
    
    DocumentQuery q2 = CORE.getPersistence().newDocumentQuery(LogEntry.MODULE_NAME, LogEntry.DOCUMENT_NAME);
    q2.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
    assertThat(q2.scalarResult(Number.class).intValue(), is(0));

    new LogEntryBuilder().owner(owner).message("text").build().upsert();

    DocumentQuery q3 = CORE.getPersistence().newDocumentQuery(LogEntry.MODULE_NAME, LogEntry.DOCUMENT_NAME);
    q3.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
    assertThat(q3.scalarResult(Number.class).intValue(), is(1));
    
    LogEntry dbRecord = CORE.getPersistence().newDocumentQuery(LogEntry.MODULE_NAME, LogEntry.DOCUMENT_NAME).beanResult();
    assertEquals(owner, dbRecord.getOwner());
    assertNotNull(dbRecord.getRunDate());
  }
}
```

Data-backed provider/service tests (H2)
---------------------------------------
- Purpose: exercise SQL-backed providers or services with realistic date/amount windows.
- Tooling: `AbstractH2Test` + `DataBuilder`; assert counts, then call provider and assert results.
```java
import org.skyve.CORE;

class ProviderH2Test extends AbstractH2Test {
  DataBuilder db = new DataBuilder().fixture(FixtureType.crud);

  @Test
  void returnsInRangeTransactions() {
    Account account = CORE.getPersistence().save(db.build(Account.MODULE_NAME, Account.DOCUMENT_NAME));

    Transaction inRange = db.factoryBuild(Transaction.MODULE_NAME, Transaction.DOCUMENT_NAME);
    inRange.setAccount(account);
    inRange.setDate(todayMinusDays(7));
    inRange.setAmount(amount(100));
    CORE.getPersistence().save(inRange);

    Transaction outOfRange = db.factoryBuild(Transaction.MODULE_NAME, Transaction.DOCUMENT_NAME);
    outOfRange.setAccount(account);
    outOfRange.setDate(todayPlusDays(7));
    outOfRange.setAmount(amount(0));
    CORE.getPersistence().save(outOfRange);

    List<Transaction> results = provider.getTransactions(account.getId(), start(), end());
    assertThat(results.size(), is(1));
  }
}
```

Verifying record counts with DocumentQuery
------------------------------------------
- Purpose: verify the number of records matching specific criteria in the database.
- Tooling: `DocumentQuery` with `AggregateFunction.Count` and `scalarResult()`.
- **Important**: Always use `addAggregateProjection()` before calling `scalarResult()`, never use `retrieveScalar()`.
```java
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

class RecordCountH2Test extends AbstractH2Test {
  DataBuilder db = new DataBuilder().fixture(FixtureType.crud);

  @Test
  void verifiesTaggedItemCounts() throws Exception {
    Tag sourceTag = CORE.getPersistence().save(db.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME));
    
    // Tag some items
    Contact contact1 = CORE.getPersistence().save(db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME));
    Contact contact2 = CORE.getPersistence().save(db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME));
    tagManager.tag(sourceTag.getBizId(), contact1);
    tagManager.tag(sourceTag.getBizId(), contact2);

    // Verify count using DocumentQuery with aggregate projection
    DocumentQuery countQuery = CORE.getPersistence().newDocumentQuery(
        Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
    countQuery.getFilter().addEquals(Tagged.tagPropertyName, sourceTag);
    countQuery.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
    long taggedCount = countQuery.scalarResult(Number.class).longValue();
    
    assertThat(taggedCount, is(2L));
  }

  @Test
  void verifiesCountWithMultipleFilters() throws Exception {
    User user = CORE.getPersistence().save(db.build(User.MODULE_NAME, User.DOCUMENT_NAME));
    
    // Create tags for this user
    Tag tag1 = CORE.getPersistence().save(db.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME));
    tag1.setBizUserId(user.getBizId());
    CORE.getPersistence().save(tag1);
    Tag tag2 = CORE.getPersistence().save(db.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME));
    tag2.setBizUserId(user.getBizId());
    CORE.getPersistence().save(tag2);

    // Count tags for this user
    DocumentQuery tagQuery = CORE.getPersistence().newDocumentQuery(
        Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
    tagQuery.getFilter().addEquals(Bean.USER_ID, user.getBizId());
    tagQuery.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
    long tagCount = tagQuery.scalarResult(Number.class).longValue();
    
    assertThat(tagCount, is(2L));
  }
}
```

**Key points:**
- Always call `addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId")` before executing the query
- Use `scalarResult(Number.class)` to get the count value, **not** `retrieveScalar()`
- The projection alias (e.g., `"CountOfId"`) is required but the name is arbitrary
- Apply filters using `getFilter().addEquals()` before adding the aggregate projection

Regression-style calculator integration
---------------------------------------
- Purpose: reproduce production issues with end-to-end calculation on realistic data.
- Tooling: H2 base, fixtures for domain graph (accounts, terms, history); assert numeric outputs and stepwise scenarios.
```java
class CalculatorH2RegressionTest extends AbstractH2Test {
  DataBuilder db = new DataBuilder().fixture(FixtureType.crud);

  @Test
  void calculatesInstallmentAfterTermsChange() {
    Account account = setupAccountWithHistory(db); // helper uses DataBuilder underneath
    Decimal result = calculator.calculate(account, nextTerms, runDate);
    assertThat(result).isEqualByComparingTo("80624.39166");

    // wind forward and recalc to ensure continuity
    advanceTerms(account, result);
    Decimal next = calculator.calculate(account, laterTerms, laterDate);
    assertThat(next).isBetween(new Decimal("26699"), new Decimal("26874.80"));
  }
}
```

Job orchestration with partial mocking
--------------------------------------
- Purpose: cover batch/job flows while stubbing heavy collaborators.
- Tooling: H2 base for persistence side effects; Mockito for calculators/external calls; verify inserts/deletes and interaction counts.
```java
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

class JobH2Test extends AbstractH2Test {
  @Mock Calculator calculator;
  @Spy @InjectMocks Job job;
  DataBuilder db = new DataBuilder().fixture(FixtureType.crud);

  @Test
  void postsBatchAndClearsQueue() {
    Batch batch = seedBatchWithUnposted(db); // helper uses DataBuilder
    when(calculator.compute(any(), anyBoolean(), any())).thenReturn(amount("123.45"));

    job.run(batch);

    DocumentQuery q1 = CORE.getPersistence().newDocumentQuery(Transaction.MODULE_NAME, Transaction.DOCUMENT_NAME);
    q1.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
    assertThat(q1.scalarResult(Number.class).intValue(), is(1));
    
    DocumentQuery q2 = CORE.getPersistence().newDocumentQuery(Unposted.MODULE_NAME, Unposted.DOCUMENT_NAME);
    q2.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
    assertThat(q2.scalarResult(Number.class).intValue(), is(0));
    
    verify(job).insertNotice(any(), any(), any(), any(), any(), any(), any());
  }
}
```

Tiny utility sanity checks
--------------------------
- Purpose: deterministic helpers (string/date/formatting).
- Tooling: plain JUnit + hamcrest/assertions; no fixtures.
```java
class UtilityTest {
  @Test
  void formatsDateOffset() {
    String expected = nowPlusDays(30).format(ISO_DATE);
    assertEquals("'" + expected + "'", Utils.constructDateAddTerm(":runDate", 30));
  }
}
```

Bizlet transformations (pre-save/validate)
------------------------------------------
- Purpose: small rules that massage a bean before save or add validation messages.
- Tooling: Mockito spies/mocks against the bean; assert mutating behaviour and message counts.
```java
class TransactionBizletTest {
  TransactionBizlet bizlet = new TransactionBizlet();
  @Spy Transaction tx = Transaction.newInstance();

  @Test
  void flipsAmountsForRecoveryTypes() throws Exception {
    tx.setType("RECOVERY");
    tx.setGrossAmount(new Decimal2("100"));

    bizlet.preSave(tx);

    assertThat(tx.getGrossAmount(), is(new Decimal2("-100")));
  }

  @Test
  void validateAddsMessageWhenDatesOverlap() throws Exception {
    ValidationException errors = new ValidationException();
    when(tx.getEffectiveFrom()).thenReturn(today());
    when(tx.getEffectiveTo()).thenReturn(today().minusDays(1));

    bizlet.validate(tx, errors);

    assertThat(errors.getMessages().size(), is(1));
  }
}
```

Service/repository CRUD smoke tests (H2)
----------------------------------------
- Purpose: prove simple existence/retrieval/update helpers over persisted data.
- Tooling: H2 base + `DataBuilder` to create related beans; assert counts before and after service calls.
```java
import org.skyve.CORE;
import jakarta.inject.Inject;

class PaymentServiceH2Test extends AbstractH2Test {
  @Inject PaymentService service;
  DataBuilder db = new DataBuilder().fixture(FixtureType.crud);

  @Test
  void updateMovesForeignKey() {
    Claim oldClaim = CORE.getPersistence().save(db.build(Claim.MODULE_NAME, Claim.DOCUMENT_NAME));
    Claim newClaim = CORE.getPersistence().save(db.build(Claim.MODULE_NAME, Claim.DOCUMENT_NAME));
    Payment payment = CORE.getPersistence().save(db.build(Payment.MODULE_NAME, Payment.DOCUMENT_NAME));
    payment.setClaim(oldClaim);
    CORE.getPersistence().save(payment);

    int moved = service.updatePaymentClaimId(oldClaim.getBizId(), newClaim.getBizId());

    assertThat(moved, is(1));
    Payment retrieved = CORE.getPersistence().retrieve(Payment.MODULE_NAME, Payment.DOCUMENT_NAME, payment.getBizId());
    assertThat(retrieved.getClaim(), is(newClaim));
  }

  @Test
  void getByCompositeKeyReturnsEmptyWhenNoMatch() {
    Accident accident = CORE.getPersistence().save(db.build(Accident.MODULE_NAME, Accident.DOCUMENT_NAME));
    List<PaymentExtension> results = service.getByAccidentNumberAndClaim(
        accident.getAccidentNumber(), accident.getClaimDetails().get(0));
    assertThat(results.size(), is(0));
  }
}
```

Search model filtering (H2)
---------------------------
- Purpose: verify search models return the expected rows with and without filters.
- Tooling: H2 base + `DataBuilder` to seed rows; set filter bean on the model and assert row contents.
```java
import org.skyve.CORE;

class AccidentSearchModelH2Test extends AbstractH2Test {
  DataBuilder db = new DataBuilder().fixture(FixtureType.crud);
  AccidentSearchModel model = new AccidentSearchModel();

  @BeforeEach
  void init() {
    model.postConstruct(CORE.getUser().getCustomer(), true);
  }

  @Test
  void filtersByPartialAccidentNumber() throws Exception {
    Accident a1 = CORE.getPersistence().save(db.build(Accident.MODULE_NAME, Accident.DOCUMENT_NAME));
    a1.setAccidentNumber("ABC123");
    CORE.getPersistence().save(a1);
    Accident a2 = CORE.getPersistence().save(db.build(Accident.MODULE_NAME, Accident.DOCUMENT_NAME));
    a2.setAccidentNumber("XYZ999");
    CORE.getPersistence().save(a2);

    ClaimSearch search = ClaimSearch.newInstance();
    search.setAccidentNumber("XYZ");
    model.setBean(search);

    List<Bean> rows = model.getRows();
    assertThat(rows.size(), is(1));
    assertThat(Binder.get(rows.get(0), Accident.accidentNumberPropertyName), is("XYZ999"));
  }
}
```

Hybrid H2 + mock seams
----------------------
- Purpose: exercise an action/service with persisted fixtures while mocking external seams (file system, persistence API, reporting, etc.).
- Tooling: `@Mock` for infrastructure objects, spy/inject mocks into the action, and assert DB side effects or thrown exceptions.
```java
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

@ExtendWith(MockitoExtension.class)
class ImportActionH2Test extends AbstractH2Test {
  @Mock Persistence persistence;
  @Mock DocumentQuery fileQuery;
  @InjectMocks @Spy ImportAction action;

  ImportRequest bean = ImportRequest.newInstance();

  @Test
  void validateModeWithNoRecordsThrowsDomainException() throws Exception {
    bean.setRunMode(RunMode.validateOnly);
    bean.setImportFileAbsolutePath("/tmp/input.txt");
    when(persistence.newDocumentQuery("FileModule", "FileDoc")).thenReturn(fileQuery);
    when(fileQuery.beanResult()).thenReturn(File.newInstance()); // nothing imported

    assertThrows(DomainException.class, () -> action.execute(bean, new MockWebContext()));
    DocumentQuery q = CORE.getPersistence().newDocumentQuery(ImportHistory.MODULE_NAME, ImportHistory.DOCUMENT_NAME);
    q.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
    assertThat(q.scalarResult(Number.class).intValue(), is(0));
  }
}
```

Relink/migration flows with aggregated errors (H2)
--------------------------------------------------
- Purpose: model inbound data replacing existing records, relinking dependants, and aggregating warnings into a domain-specific exception.
- Tooling: H2 fixtures via `DataBuilder` to create old/new graphs; custom exception assertions against collected problems; verify dependent rows moved or left intact.
```java
import org.skyve.CORE;
import jakarta.inject.Inject;

class RelinkJobH2Test extends AbstractH2Test {
  @Spy @Inject RelinkJob job;
  DataBuilder db = new DataBuilder().fixture(FixtureType.crud);

  @Test
  void blocksRelinkWhenDependentExistsForDifferentKey() {
    ExistingRecord existing = CORE.getPersistence().save(db.build(ExistingRecord.MODULE_NAME, ExistingRecord.DOCUMENT_NAME));
    Dependent dep = CORE.getPersistence().save(db.build(Dependent.MODULE_NAME, Dependent.DOCUMENT_NAME));
    dep.setOwner(existing);
    CORE.getPersistence().save(dep);

    IncomingRecord incoming = db.build(ExistingRecord.MODULE_NAME, ExistingRecord.DOCUMENT_NAME);
    incoming.setExternalKey(existing.getExternalKey());

    RelinkException ex = assertThrows(RelinkException.class,
        () -> job.relinkDependants(existing, incoming));
    assertThat(ex.getProblems().size(), is(1));
    assertThat(ex.getProblems().iterator().next().getWhat(), containsString("existing dependent"));
  }

  @Test
  void relinksDependantsAndDeletesOldOwner() {
    ExistingRecord existing = CORE.getPersistence().save(db.build(ExistingRecord.MODULE_NAME, ExistingRecord.DOCUMENT_NAME));
    Dependent dep = CORE.getPersistence().save(db.build(Dependent.MODULE_NAME, Dependent.DOCUMENT_NAME));
    dep.setOwner(existing);
    CORE.getPersistence().save(dep);

    IncomingRecord incoming = CORE.getPersistence().save(db.build(ExistingRecord.MODULE_NAME, ExistingRecord.DOCUMENT_NAME));
    incoming.setExternalKey(existing.getExternalKey());
    CORE.getPersistence().save(incoming);

    job.relinkDependants(existing, incoming);

    ExistingRecord retrievedExisting = CORE.getPersistence().retrieve(ExistingRecord.MODULE_NAME, ExistingRecord.DOCUMENT_NAME, existing.getBizId());
    assertThat(retrievedExisting, is(nullValue()));
    Dependent retrievedDep = CORE.getPersistence().retrieve(Dependent.MODULE_NAME, Dependent.DOCUMENT_NAME, dep.getBizId());
    assertThat(retrievedDep.getOwner(), is(incoming));
  }
}
```

Field parsing with binding metadata
-----------------------------------
- Purpose: ensure fixed-width/import strings are parsed into numeric or date fields via binding metadata, including sign handling.
- Tooling: `DataBuilder` to create the domain bean and its field metadata; call the field setter with import strings and assert stored values.
```java
class DecimalFieldParsingTest extends AbstractH2Test {
  DataBuilder db = new DataBuilder().fixture(FixtureType.crud);

  @Test
  void parsesPaddedSignedNumbers() {
    Totals bean = db.factoryBuild(Totals.MODULE_NAME, Totals.DOCUMENT_NAME);
    FieldExtension field = db.factoryBuild(Field.MODULE_NAME, Field.DOCUMENT_NAME);
    field.setBindingName(Totals.totalAmountPropertyName);
    field.setType(Type.decimal);

    field.set(bean, "000000023.20");
    assertThat(bean.getTotalAmount(), is(new Decimal2("23.20")));

    field.set(bean, "000000023.20-");
    assertThat(bean.getTotalAmount(), is(new Decimal2("-23.20")));
  }
}
```

Choosing a pattern
------------------
1) Pure logic/branching → Mockito-only.  
2) Validation rules → Mockito with `ValidationException` assertions.  
3) Persistence side effects/queries → H2 integration with `DataBuilder` fixtures.  
4) Reproducing production defects/end-to-end flows → H2 with realistic data and multi-step runs.  
5) Small helpers → plain deterministic assertions.  
6) Hybrid flows → H2 + mocks.
