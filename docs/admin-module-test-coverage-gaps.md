# Admin Module Test Coverage Gaps

This document identifies areas of poor code coverage in the `src/main/java/modules/admin` package that need unit tests.

## Summary

- **Total Actions**: 77 identified
- **Tested Actions**: 11 (MakePasswordChange, Check, GeneratePassword, CopyTagToUser, Communication actions: AddUnsubscribeLink, TestSend, CreateFiles, GetCount, GetResults, SendNow, DeleteBatch, ControlPanel: CacheStats)
- **Untested Actions**: ~66

- **Total Bizlets**: 31 identified
- **Tested Bizlets**: 3 (UserBizlet, ConfigurationBizlet, StartupBizlet)
- **Untested Bizlets**: ~28

- **Total Extensions**: 80 identified
- **Tested Extensions**: 5 (UserExtension, ConfigurationExtension, StartupExtension, ReportDatasetExtension, UserProxyExtension)
- **Untested Extensions**: ~75

- **Total Services**: 15 identified
- **Tested Services**: 2 (UserService, CommunicationService)
- **Untested Services**: ~13

- **Total Jobs**: 26 identified
- **Tested Jobs**: 0
- **Untested Jobs**: ~26

---

## 1. Actions (ServerSideAction implementations)

### Communication Actions
- [x] `modules.admin.Communication.actions.AddUnsubscribeLink`
- [x] `modules.admin.Communication.actions.TestSend` - **HAS TEST** (TestSendH2Test)
- [x] `modules.admin.Communication.actions.CreateFiles` - **HAS TEST** (CreateFilesH2Test)
- [x] `modules.admin.Communication.actions.GetCount` - **HAS TEST** (GetCountH2Test)
- [x] `modules.admin.Communication.actions.GetResults` - **HAS TEST** (GetResultsH2Test)
- [x] `modules.admin.Communication.actions.SendNow` - **HAS TEST** (SendNowH2Test)
- [x] `modules.admin.Communication.actions.DeleteBatch` - **HAS TEST** (DeleteBatchH2Test)

### ControlPanel Actions
- [x] `modules.admin.ControlPanel.actions.CacheStats` - **HAS TEST** (CacheStatsTest)
- [ ] `modules.admin.ControlPanel.actions.EvictFromCache`
- [ ] `modules.admin.ControlPanel.actions.EvictSelectedCache`
- [ ] `modules.admin.ControlPanel.actions.StopOrStartSelectedCache`
- [ ] `modules.admin.ControlPanel.actions.SwapCustomer`
- [ ] `modules.admin.ControlPanel.actions.ExecuteQuery`
- [ ] `modules.admin.ControlPanel.actions.ExecuteSAIL`
- [ ] `modules.admin.ControlPanel.actions.GenerateMenuSAIL`
- [ ] `modules.admin.ControlPanel.actions.GenerateModuleSAIL`
- [ ] `modules.admin.ControlPanel.actions.DownloadResults`
- [ ] `modules.admin.ControlPanel.actions.DownloadSAIL`
- [ ] `modules.admin.ControlPanel.actions.DownloadClientSAIL`
- [ ] `modules.admin.ControlPanel.actions.GenerateTestData`
- [ ] `modules.admin.ControlPanel.actions.DeleteTestData`

### DataMaintenance Actions
- [ ] `modules.admin.DataMaintenance.actions.Backup`
- [ ] `modules.admin.DataMaintenance.actions.BackupSelected`
- [ ] `modules.admin.DataMaintenance.actions.CheckContent`
- [ ] `modules.admin.DataMaintenance.actions.CheckAuditMatches`
- [ ] `modules.admin.DataMaintenance.actions.CollectContentGarbage`
- [ ] `modules.admin.DataMaintenance.actions.ContentSelected`
- [ ] `modules.admin.DataMaintenance.actions.Create`
- [ ] `modules.admin.DataMaintenance.actions.DataMaintenanceExportAction`
- [ ] `modules.admin.DataMaintenance.actions.DataMaintenanceImportAction`
- [ ] `modules.admin.DataMaintenance.actions.DeleteBackup`
- [ ] `modules.admin.DataMaintenance.actions.DownloadBackup`
- [ ] `modules.admin.DataMaintenance.actions.Drop`
- [ ] `modules.admin.DataMaintenance.actions.DropIndexing`
- [ ] `modules.admin.DataMaintenance.actions.RefreshBackupList`
- [ ] `modules.admin.DataMaintenance.actions.RefreshDocumentTuples`
- [ ] `modules.admin.DataMaintenance.actions.Reindex`
- [ ] `modules.admin.DataMaintenance.actions.ReindexContent`
- [ ] `modules.admin.DataMaintenance.actions.ReindexData`
- [ ] `modules.admin.DataMaintenance.actions.Restore`
- [ ] `modules.admin.DataMaintenance.actions.Sync`
- [ ] `modules.admin.DataMaintenance.actions.Truncate`
- [ ] `modules.admin.DataMaintenance.actions.TruncateAuditLog`
- [ ] `modules.admin.DataMaintenance.actions.UploadBackup`

### DocumentCreator Actions
- [ ] `modules.admin.DocumentCreator.actions.Submit`
- [ ] `modules.admin.DocumentCreator.actions.UpdatePreview`

### ImportExport Actions
- [ ] `modules.admin.ImportExport.actions.ClearImport`
- [ ] `modules.admin.ImportExport.actions.GetImportTemplate`
- [ ] `modules.admin.ImportExport.actions.ResetColumns`
- [ ] `modules.admin.ImportExport.actions.RunExport`
- [ ] `modules.admin.ImportExport.actions.RunImport`
- [ ] `modules.admin.ImportExport.actions.UploadSimpleImportDataFile`

### Job Actions
- [ ] `modules.admin.Job.actions.CancelJob`
- [ ] `modules.admin.Job.actions.RerunJob`

### Jobs Actions
- [ ] `modules.admin.Jobs.actions.DeleteCompletedJobs`
- [ ] `modules.admin.Jobs.actions.Refresh`

### JobSchedule Actions
- [ ] `modules.admin.JobSchedule.actions.RunJobNow`

### MonitoringDashboard Actions
- [ ] `modules.admin.MonitoringDashboard.actions.Save`
- [ ] `modules.admin.MonitoringDashboard.actions.Purge`

### ReportDataset Actions
- [ ] `modules.admin.ReportDataset.actions.TestQuery`

### ReportManager Actions
- [ ] `modules.admin.ReportManager.actions.ExportReportSpecifications`
- [ ] `modules.admin.ReportManager.actions.ImportReportSpecifications`

### ReportTemplate Actions
- [ ] `modules.admin.ReportTemplate.actions.AddUserToEmail`
- [ ] `modules.admin.ReportTemplate.actions.Back`
- [ ] `modules.admin.ReportTemplate.actions.CopyReport`
- [ ] `modules.admin.ReportTemplate.actions.DownloadJasperReport`
- [ ] `modules.admin.ReportTemplate.actions.DownloadReport`
- [ ] `modules.admin.ReportTemplate.actions.Next`
- [ ] `modules.admin.ReportTemplate.actions.TestJasperReport`

### SelfRegistration Actions
- [ ] `modules.admin.SelfRegistration.actions.Register`
- [ ] `modules.admin.SelfRegistration.actions.ResendActivation`

### Snapshot Actions
- [ ] `modules.admin.Snapshot.actions.CopySnapshotToUser`

### Snapshots Actions
- [ ] `modules.admin.Snapshots.actions.Reorder`

### Startup Actions
- [ ] `modules.admin.Startup.actions.Dismiss`
- [ ] `modules.admin.Startup.actions.SaveChanges`

### Tag Actions
- [ ] `modules.admin.Tag.actions.BulkDocumentAction`
- [ ] `modules.admin.Tag.actions.Clear`
- [ ] `modules.admin.Tag.actions.PerformCombination`
- [ ] `modules.admin.Tag.actions.PrepareExplanation`
- [ ] `modules.admin.Tag.actions.TagAll`
- [ ] `modules.admin.Tag.actions.UploadTagCriteria`
- [x] `modules.admin.Tag.actions.CopyTagToUser` - **HAS TEST** (CopyTagToUserH2Test)

### User Actions
- [ ] `modules.admin.User.actions.Back`
- [x] `modules.admin.User.actions.Check` - **HAS TEST** (CheckTest)
- [x] `modules.admin.User.actions.GeneratePassword` - **HAS TEST** (GeneratePasswordActionTest)
- [ ] `modules.admin.User.actions.GenerateUniqueUserName`
- [ ] `modules.admin.User.actions.New`
- [ ] `modules.admin.User.actions.Next`
- [ ] `modules.admin.User.actions.ResendActivation`

### UserCandidateContact Actions
- [ ] `modules.admin.UserCandidateContact.actions.Select`

### UserDashboard Actions
- [ ] `modules.admin.UserDashboard.actions.UpdateMyDetails`

### UserList Actions
- [ ] `modules.admin.UserList.actions.CreateUsers`
- [ ] `modules.admin.UserList.actions.InviteUsers`

### Configuration Actions
- [ ] `modules.admin.Configuration.actions.SendMail`

### ChangePassword Actions
- [x] `modules.admin.ChangePassword.actions.MakePasswordChange` - **HAS TEST** (MakePasswordChangeTest)

---

## 2. Bizlets

- [ ] `modules.admin.Audit.AuditBizlet`
- [ ] `modules.admin.ChangePassword.ChangePasswordBizlet`
- [ ] `modules.admin.Communication.CommunicationBizlet`
- [x] `modules.admin.Configuration.ConfigurationBizlet` - **HAS TEST** (ConfigurationBizletTest)
- [ ] `modules.admin.ControlPanel.ControlPanelBizlet`
- [ ] `modules.admin.Country.CountryBizlet`
- [ ] `modules.admin.DocumentCreator.DocumentCreatorBizlet`
- [ ] `modules.admin.Group.GroupBizlet`
- [ ] `modules.admin.GroupRole.GroupRoleBizlet`
- [ ] `modules.admin.ImportExport.ImportExportBizlet`
- [ ] `modules.admin.ImportExportColumn.ImportExportColumnBizlet`
- [ ] `modules.admin.JobSchedule.JobScheduleBizlet`
- [ ] `modules.admin.Jobs.JobsBizlet`
- [ ] `modules.admin.ModuleDocument.ModuleDocumentBizlet`
- [ ] `modules.admin.ReportDataset.ReportDatasetBizlet`
- [ ] `modules.admin.ReportParameter.ReportParameterBizlet`
- [ ] `modules.admin.ReportTemplate.ReportTemplateBizlet`
- [ ] `modules.admin.SecurityLog.SecurityLogBizlet`
- [ ] `modules.admin.SelfRegistration.SelfRegistrationBizlet`
- [ ] `modules.admin.SelfRegistrationActivation.SelfRegistrationActivationBizlet`
- [ ] `modules.admin.Snapshot.SnapshotBizlet`
- [ ] `modules.admin.Snapshots.SnapshotsBizlet`
- [x] `modules.admin.Startup.StartupBizlet` - **HAS TEST** (StartupBizletH2Test)
- [ ] `modules.admin.SystemDashboard.SystemDashboardBizlet`
- [ ] `modules.admin.Tag.TagBizlet`
- [ ] `modules.admin.UserAccount.UserAccountBizlet`
- [ ] `modules.admin.UserDashboard.UserDashboardBizlet`
- [ ] `modules.admin.UserList.UserListBizlet`
- [ ] `modules.admin.UserLoginRecord.UserLoginRecordBizlet`
- [ ] `modules.admin.UserRole.UserRoleBizlet`
- [x] `modules.admin.User.UserBizlet` - **HAS TEST** (UserBizletTest)
- [ ] `modules.admin.CommunicationTemplate.CommunicationTemplateBizlet`

---

## 3. Extensions

- [ ] `modules.admin.AuditList.AuditListExtension`
- [ ] `modules.admin.Communication.CommunicationExtension`
- [x] `modules.admin.Configuration.ConfigurationExtension` - **HAS TEST** (ConfigurationExtensionTest)
- [ ] `modules.admin.Contact.ContactExtension`
- [ ] `modules.admin.ControlPanel.ControlPanelExtension`
- [ ] `modules.admin.Country.CountryExtension`
- [ ] `modules.admin.DataMaintenance.DataMaintenanceExtension`
- [ ] `modules.admin.ImportExport.ImportExportExtension`
- [ ] `modules.admin.Job.JobExtension`
- [ ] `modules.admin.JobSchedule.JobScheduleExtension`
- [ ] `modules.admin.ReportDataset.ReportDatasetExtension`
- [x] `modules.admin.ReportDataset.ReportDatasetExtension` - **HAS TEST** (ReportDatasetExtensionTest)
- [ ] `modules.admin.ReportManager.ReportManagerExtension`
- [ ] `modules.admin.ReportParameter.ReportParameterExtension`
- [ ] `modules.admin.ReportTemplate.ReportTemplateExtension`
- [ ] `modules.admin.SecurityLog.SecurityLogExtension`
- [ ] `modules.admin.SelfRegistration.SelfRegistrationExtension`
- [ ] `modules.admin.SelfRegistrationActivation.SelfRegistrationActivationExtension`
- [x] `modules.admin.Startup.StartupExtension` - **HAS TEST** (StartupExtensionTest)
- [ ] `modules.admin.Tag.TagExtension`
- [ ] `modules.admin.UserDashboard.UserDashboardExtension`
- [x] `modules.admin.User.UserExtension` - **HAS TEST** (UserExtensionTest, UserExtensionH2Test)
- [x] `modules.admin.UserProxy.UserProxyExtension` - **HAS TEST** (UserProxyExtensionTest)

---

## 4. Services

- [ ] `modules.admin.Audit.AuditService`
- [x] `modules.admin.Communication.CommunicationService` - **HAS TEST** (CommunicationServiceTest)
- [ ] `modules.admin.Contact.ContactService`
- [ ] `modules.admin.Country.CountryService`
- [ ] `modules.admin.DataGroup.DataGroupService`
- [ ] `modules.admin.DataMaintenance.DataMaintenanceService`
- [ ] `modules.admin.Group.GroupService`
- [ ] `modules.admin.ImportExport.ImportExportService`
- [ ] `modules.admin.Jobs.JobsService`
- [ ] `modules.admin.ReportManager.ReportManagerService`
- [ ] `modules.admin.ReportDataset.ReportDatasetService`
- [ ] `modules.admin.Snapshot.SnapshotService`
- [ ] `modules.admin.Subscription.SubscriptionService`
- [ ] `modules.admin.Tag.TagService`
- [x] `modules.admin.User.UserService` - **HAS TEST** (UserServiceH2Test)

---

## 5. Jobs

- [ ] `modules.admin.Configuration.AvailableDiskSpaceAlarmJob`
- [ ] `modules.admin.ChangePassword.SendPasswordChangeNotificationJob`
- [ ] `modules.admin.Communication.ProcessCommunicationForTagJob`
- [ ] `modules.admin.ControlPanel.GenerateTestDataJob`
- [ ] `modules.admin.DataMaintenance.BackupJob`
- [ ] `modules.admin.DataMaintenance.RefreshDocumentTuplesJob`
- [ ] `modules.admin.DataMaintenance.TruncateAuditLogJob`
- [ ] `modules.admin.ReportTemplate.jobs.ReportJob`
- [ ] `modules.admin.Tag.DeleteAllTaggedDataForTagJob`
- [ ] `modules.admin.Tag.PerformDocumentActionForTagJob`
- [ ] `modules.admin.UserList.BulkUserCreationJob`
- [ ] `modules.admin.UserLoginRecord.jobs.DifferentCountryLoginNotificationJob`

---

## 6. Utility Classes

- [ ] `modules.admin.AuditJSONGenerator`
- [ ] `modules.admin.LoggingInterceptor`
- [ ] `modules.admin.ModulesUtil` - **HAS TEST** (ModulesUtilTest)
- [ ] `modules.admin.PasswordGenerator`
- [ ] `modules.admin.RDBMSAuditInterceptor`

---

## 7. Factories

Factories are typically test utilities and may not need direct unit tests, but they should be verified through integration tests:
- [ ] `modules.admin.Audit.AuditFactory`
- [ ] `modules.admin.ChangePassword.ChangePasswordFactory`
- [ ] `modules.admin.Communication.CommunicationFactory`
- [ ] `modules.admin.Configuration.ConfigurationFactory`
- [ ] `modules.admin.Contact.ContactFactory`
- [ ] `modules.admin.ControlPanel.ControlPanelFactory`
- [ ] `modules.admin.DataMaintenance.DataMaintenanceFactory`
- [ ] `modules.admin.DocumentCreator.DocumentCreatorFactory`
- [ ] `modules.admin.DocumentNumber.DocumentNumberFactory`
- [ ] `modules.admin.DynamicEntity.DynamicEntityFactory`
- [ ] `modules.admin.DynamicRelation.DynamicRelationFactory`
- [ ] `modules.admin.ImportExport.ImportExportFactory`
- [ ] `modules.admin.Job.JobFactory`
- [ ] `modules.admin.JobSchedule.JobScheduleFactory`
- [ ] `modules.admin.Jobs.JobsFactory`
- [ ] `modules.admin.ReportDataset.ReportDatasetFactory`
- [ ] `modules.admin.ReportTemplate.ReportTemplateFactory`
- [ ] `modules.admin.SelfRegistration.SelfRegistrationFactory`
- [ ] `modules.admin.Snapshot.SnapshotFactory`
- [ ] `modules.admin.Startup.StartupFactory`
- [ ] `modules.admin.Subscription.SubscriptionFactory`
- [ ] `modules.admin.Tag.TagFactory`
- [ ] `modules.admin.User.UserFactory`
- [ ] `modules.admin.UserCandidateContact.UserCandidateContactFactory`
- [ ] `modules.admin.UserDashboard.UserDashboardFactory`

---

## 8. Models

Model classes in the MonitoringDashboard and SystemDashboard packages:
- [ ] `modules.admin.MonitoringDashboard.models.*` (various model classes)
- [ ] `modules.admin.SystemDashboard.models.*` (various model classes)
- [ ] `modules.admin.SecurityLog.models.GeoIPMap`
- [ ] `modules.admin.UserLoginRecord.models.GeoIPMap`
- [ ] `modules.admin.Communication.models.BatchesModel`
- [ ] `modules.admin.DataMaintenance.models.BackupsModel`
- [ ] `modules.admin.DataMaintenance.models.ContentModel`
- [ ] `modules.admin.Audit.models.AuditComparisonModel`
- [ ] `modules.admin.AuditList.models.ArchivedAuditListModel`

---

## Priority Recommendations

### High Priority (Core Functionality)
1. **Services** - These contain business logic and should be thoroughly tested:
   - `AuditService`
   - `DataMaintenanceService`
   - `ImportExportService`
   - `TagService`
   - `SnapshotService`

2. **Critical Actions** - User-facing and system-critical:
   - `SelfRegistration.actions.Register`
   - `DataMaintenance.actions.Backup`
   - `DataMaintenance.actions.Restore`
   - `ImportExport.actions.RunImport`
   - `ImportExport.actions.RunExport`

3. **Bizlets** - Business logic layer:
   - `DataMaintenanceBizlet`
   - `ImportExportBizlet`
   - `TagBizlet`
   - `CommunicationBizlet`

### Medium Priority
4. **ControlPanel Actions** - Admin functionality
5. **Job Classes** - Background processing
6. **Utility Classes** - `PasswordGenerator`, `RDBMSAuditInterceptor`

### Lower Priority
7. **Factories** - Test utilities (verify through integration tests)
8. **Models** - Data transfer objects (test through integration)

---

## Notes

### Test Naming Conventions

Follow Skyve naming conventions to clearly indicate test type:
- **Mock tests** (pure Mockito unit tests): `{ClassName}Test`, e.g. `TagTest`
- **H2 tests** (tests extending `AbstractH2Test`): `{ClassName}H2Test`, e.g. `TagH2Test`
- **Integration tests**: `{ClassName}IT`, e.g. `TagIT`

Place tests in: `src/test/java/modules/admin/{package}/{ClassName}Test.java`

### Fixtures and DataBuilder Rules

**Critical Rules:**
- **Never use `new` to instantiate a Skyve document.** Always use `MyDocument.newInstance()` or `DataBuilder`.
- **H2 tests** must use `DataBuilder` or factory fixtures for all domain object creation:
  ```java
  DataBuilder db = new DataBuilder().fixture(FixtureType.crud);
  Account account = CORE.getPersistence().save(db.build(Account.MODULE_NAME, Account.DOCUMENT_NAME));
  ```
- **Mockito-only tests** must mock Skyve documents; avoid calling `newInstance()` unless strictly required.
- Use `db.build(<MODULE>, <DOC>)` for general objects, and `db.factoryBuild(...)` for complex or linked fixtures.

### Choosing the Right Test Pattern

1. **Pure logic/branching** → Mockito-only (`@ExtendWith(MockitoExtension.class)`)
   - Use for: Actions with simple logic, utility classes, validation rules
   - Example: `PasswordGenerator`, simple action classes

2. **Validation rules** → Mockito with `ValidationException` assertions
   - Use for: Bizlet `validate()` methods, action `preExecute()` validation
   - Assert on `ValidationException` contents and message counts

3. **Persistence side effects/queries** → H2 integration with `DataBuilder` fixtures
   - Use for: Services, actions that modify data, queries
   - Extend `AbstractH2Test` and use `DataBuilder` for fixtures

4. **Reproducing production defects/end-to-end flows** → H2 with realistic data
   - Use for: Complex workflows, regression tests, multi-step scenarios

5. **Small helpers** → Plain deterministic assertions
   - Use for: Utility classes with no dependencies (e.g., `PasswordGenerator`)

6. **Hybrid flows** → H2 + mocks
   - Use for: Actions/services with persisted fixtures but external dependencies
   - Mock infrastructure (file system, external APIs) while using H2 for persistence

### Specific Patterns by Class Type

#### Actions (ServerSideAction)
- **Simple actions** (no persistence): Mockito-only with `@Mock` for dependencies
- **Actions with persistence**: H2 test with `DataBuilder` fixtures
- **Actions with validation**: Mockito test asserting `ValidationException`
- **Actions with external dependencies**: H2 + mocks hybrid approach

#### Bizlets
- **Validation logic**: Mockito test with `@Spy` bean, assert `ValidationException`
- **Pre-save transformations**: Mockito test with `@Spy` bean, assert mutations
- **Domain value providers**: H2 test with `DataBuilder` to seed data
- **Complex bizlets**: H2 test for persistence side effects

#### Services
- **CRUD operations**: H2 test with `DataBuilder` fixtures
- **Query methods**: H2 test with seeded data, assert results
- **Business logic**: H2 test with realistic data scenarios
- Use `@Inject` for service injection in H2 tests

#### Extensions
- **Simple extensions**: Mockito test if no complex logic
- **Extensions with business logic**: H2 test if persistence involved
- **Extensions with validation**: Mockito test with validation assertions

#### Jobs
- **Job orchestration**: H2 test with partial mocking
- Use `DataBuilder` to seed data, mock heavy collaborators
- Verify database side effects (inserts/deletes) using `DocumentQuery` with aggregate counts

#### Utility Classes
- **Pure utilities**: Plain JUnit with deterministic assertions
- **Utilities with dependencies**: Mockito test with mocked dependencies

### Verifying Record Counts

When testing persistence side effects, use `DocumentQuery` with aggregate projections:

```java
DocumentQuery q = CORE.getPersistence().newDocumentQuery(MyDoc.MODULE_NAME, MyDoc.DOCUMENT_NAME);
q.getFilter().addEquals(MyDoc.somePropertyPropertyName, value);
q.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
long count = q.scalarResult(Number.class).longValue();
assertThat(count, is(expectedValue));
```

**Important**: Always use `addAggregateProjection()` before calling `scalarResult()`, never use `retrieveScalar()`.

### Example Test Structures

**Mockito-only Action Test:**
```java
@ExtendWith(MockitoExtension.class)
class MyActionTest {
  @Mock Dependency dep;
  @Spy @InjectMocks MyAction action;
  
  @Test
  void testExecute() {
    // test logic
  }
}
```

**H2 Service Test:**
```java
class MyServiceH2Test extends AbstractH2Test {
  @Inject MyService service;
  DataBuilder db = new DataBuilder().fixture(FixtureType.crud);
  
  @Test
  void testServiceMethod() {
    // use db.build() for fixtures
    // assert persistence side effects
  }
}
```

**Bizlet Validation Test:**
```java
@ExtendWith(MockitoExtension.class)
class MyBizletTest {
  MyBizlet bizlet = new MyBizlet();
  @Spy MyBean bean = MyBean.newInstance();
  
  @Test
  void validateAddsMessageWhenInvalid() throws Exception {
    ValidationException errors = new ValidationException();
    // setup bean state
    bizlet.validate(bean, errors);
    assertThat(errors.getMessages().size(), is(1));
  }
}
```

### Integration Tests

Consider integration tests (`*IT.java`) for:
- Complex workflows involving multiple actions
- Jobs that require full system context
- Services with external dependencies
- End-to-end user flows

### Additional Resources

See `docs/test-patterns.md` for detailed examples of all test patterns and more comprehensive guidance.
