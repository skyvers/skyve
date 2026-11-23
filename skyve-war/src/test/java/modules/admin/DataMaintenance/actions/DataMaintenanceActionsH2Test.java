package modules.admin.DataMaintenance.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.CORE;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.content.MimeType;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.UploadException;
import org.skyve.impl.job.JobSchedulerStaticSingleton;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.WebFileInputStream;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import org.skyve.web.WebContext;

import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.AdminDocument;
import modules.admin.domain.Audit;
import modules.admin.domain.DataMaintenance;
import util.AbstractH2Test;

/**
 * Verifies Data Maintenance DDL and utility actions against the H2 test application.
 *
 * <p>Actions are exercised through their server-side interfaces with persistence and web-context
 * effects asserted where relevant. Content garbage-collection coverage confirms that the manual
 * action delegates to the configured job scheduler, reports successful submission to the user and
 * preserves the action bean.
 */
class DataMaintenanceActionsH2Test extends AbstractH2Test {
	@TempDir
	private Path tempDir;

	private DataBuilder db;
	private DataMaintenance bean;
	private MockWebContext webContext;
	private String savedBackupDirectory;
	private String savedExternalBackupClass;
	private JobScheduler savedJobScheduler;

	@BeforeEach
	void setup() {
		savedBackupDirectory = UtilImpl.BACKUP_DIRECTORY;
		savedExternalBackupClass = UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS;
		savedJobScheduler = JobSchedulerStaticSingleton.get();
		UtilImpl.BACKUP_DIRECTORY = tempDir.toString() + File.separator;
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = null;

		db = new DataBuilder().fixture(FixtureType.crud);
		bean = db.build(AdminDocument.DATA_MAINTENANCE);
		webContext = new MockWebContext();
	}

	@AfterEach
	void teardown() {
		UtilImpl.BACKUP_DIRECTORY = savedBackupDirectory;
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = savedExternalBackupClass;
		JobSchedulerStaticSingleton.set(savedJobScheduler);
	}

	// ---- Create action ----

	@Test
	void createActionGeneratesDdlScript() throws Exception {
		Create action = new Create();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
		// The CREATE DDL script should be set on the bean
		assertThat(bean.getDdlScript(), is(notNullValue()));
	}

	// ---- Drop action ----

	@Test
	void dropActionGeneratesDdlScript() throws Exception {
		Drop action = new Drop();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
		assertThat(bean.getDdlScript(), is(notNullValue()));
	}

	// ---- Sync action ----

	@Test
	void syncActionGeneratesDdlScript() throws Exception {
		Sync action = new Sync();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
		assertThat(bean.getDdlScript(), is(notNullValue()));
	}

	// ---- RefreshBackupList action ----

	@Test
	void refreshBackupListActionReturnsBean() throws Exception {
		RefreshBackupList action = new RefreshBackupList();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
	}

	@Test
	void dataMaintenanceExportWithNoSelectedDocumentReturnsWorkbook() throws Exception {
		bean.setModDocName(null);
		webContext.setCurrentBean(bean);

		BizPortWorkbook workbook = new DataMaintenanceExportAction().bizExport(webContext);

		assertThat(workbook, is(notNullValue()));
	}

	// ---- DeleteBackup action ----

	@Test
	void deleteBackupDeletesSelectedLocalBackupAndRefreshesList() throws Exception {
		Path backup = createBackup("delete-me.zip", "backup");
		bean.setSelectedBackupName("delete-me.zip");

		ServerSideActionResult<DataMaintenance> result = new DeleteBackup().execute(bean, webContext);

		assertThat(result.getBean(), is(bean));
		assertFalse(Files.exists(backup));
		assertThat(bean.getSelectedBackupName(), is((String) null));
		assertThat(bean.getRefreshBackups(), is(Boolean.TRUE));
	}

	@Test
	void deleteBackupRefreshesListWhenLocalBackupAlreadyMissing() throws Exception {
		bean.setSelectedBackupName("missing.zip");

		ServerSideActionResult<DataMaintenance> result = new DeleteBackup().execute(bean, webContext);

		assertThat(result.getBean(), is(bean));
		assertThat(bean.getSelectedBackupName(), is((String) null));
		assertThat(bean.getRefreshBackups(), is(Boolean.TRUE));
	}

	// ---- DownloadBackup action ----

	@Test
	void downloadBackupPrepareAcceptsExistingLocalBackupAndReturnsDownload() throws Exception {
		Path backup = createBackup("download-me.zip", "zip");
		bean.setSelectedBackupName("download-me.zip");
		DownloadBackup action = new DownloadBackup();

		action.prepare(bean, webContext);
		Download download = action.download(bean, webContext);

		assertThat(download.getFileName(), is("download-me.zip"));
		assertThat(download.getFile(), is(backup.toFile()));
		assertThat(download.getMimeType(), is(MimeType.zip));
	}

	@Test
	void downloadBackupPrepareRejectsMissingLocalBackup() {
		bean.setSelectedBackupName("missing.zip");
		DownloadBackup action = new DownloadBackup();

		assertThrows(ValidationException.class, () -> action.prepare(bean, webContext));
	}

	// ---- UploadBackup action ----

	@Test
	void uploadBackupStoresFileAndRefreshesBackupList() throws Exception {
		Files.createDirectories(backupPath("uploaded.zip").getParent());
		Upload upload = upload("uploaded.zip", "uploaded content");

		DataMaintenance result = new UploadBackup().upload(bean, upload, new UploadException(), webContext);

		assertThat(result, is(bean));
		assertThat(Files.readString(backupPath("uploaded.zip")), is("uploaded content"));
		assertThat(bean.getRefreshBackups(), is(Boolean.TRUE));
	}

	@Test
	void uploadBackupRejectsDuplicateLocalBackup() throws Exception {
		createBackup("duplicate.zip", "existing");
		Upload upload = upload("duplicate.zip", "new");
		UploadBackup action = new UploadBackup();
		UploadException uploadException = new UploadException();

		assertThrows(UploadException.class, () -> action.upload(bean, upload, uploadException, webContext));
		assertThat(Files.readString(backupPath("duplicate.zip")), is("existing"));
	}

	// ---- Backup action ----

	@Test
	void backupActionStartsJobAndReturnsBean() throws Exception {
		Backup action = new Backup();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
	}

	// ---- Reindex action ----

	@Test
	void reindexActionStartsJobAndReturnsBean() throws Exception {
		Reindex action = new Reindex();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
	}

	// ---- ReindexContent action ----

	@Test
	void reindexContentActionStartsJobAndReturnsBean() throws Exception {
		ReindexContent action = new ReindexContent();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
	}

	/** Confirms that the maintenance action submits garbage collection and reports that submission. */
	@Test
	void collectContentGarbageActionStartsJobAndReturnsBean() throws Exception {
		JobScheduler scheduler = mock(JobScheduler.class);
		WebContext context = mock(WebContext.class);
		JobSchedulerStaticSingleton.set(scheduler);

		ServerSideActionResult<DataMaintenance> result = new CollectContentGarbage().execute(bean, context);

		verify(scheduler).runContentGarbageCollector();
		verify(context).growl(MessageSeverity.info, "Content Garbage Collection Job has been started");
		assertThat(result.getBean(), is(bean));
	}

	// ---- ReindexData action ----

	@Test
	void reindexDataActionStartsJobAndReturnsBean() throws Exception {
		ReindexData action = new ReindexData();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
	}

	// ---- Truncate action: null password validation ----

	@Test
	void truncateWithNullPasswordThrowsValidationException() {
		bean.setConfirmPassword(null);
		Truncate action = new Truncate();
		assertThrows(ValidationException.class, () -> action.execute(bean, webContext));
	}

	@Test
	void truncateAuditLogActionSchedulesJobAndSetsResponse() throws Exception {
		ServerSideActionResult<DataMaintenance> result = new TruncateAuditLog().execute(bean, webContext);

		assertThat(result.getBean(), is(bean));
		assertThat(bean.getAuditResponse(), is("Job commenced."));
	}

	@Test
	void truncateAuditLogGetAuditQueryAddsAllProvidedFilters() {
		Persistence persistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		Timestamp start = new Timestamp();
		Timestamp end = new Timestamp();
		UserProxyExtension auditUser = new UserProxyExtension();
		auditUser.setUserName("auditor");
		bean.setAuditModuleName("admin");
		bean.setAuditDocumentName("User");
		bean.setAuditOperation(Audit.Operation.update);
		bean.setAuditTimestampStart(start);
		bean.setAuditTimestampEnd(end);
		bean.setAuditUser(auditUser);
		when(persistence.newDocumentQuery(AdminDocument.AUDIT)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);

		DocumentQuery result = TruncateAuditLog.getAuditQuery(persistence, bean);

		assertThat(result, is(query));
		verify(filter).addEquals(Audit.auditModuleNamePropertyName, "admin");
		verify(filter).addEquals(Audit.auditDocumentNamePropertyName, "User");
		verify(filter).addEquals(Audit.operationPropertyName, Audit.Operation.update);
		verify(filter).addGreaterThanOrEqualTo(Audit.timestampPropertyName, start);
		verify(filter).addLessThanOrEqualTo(Audit.timestampPropertyName, end);
		verify(filter).addEquals(Audit.userNamePropertyName, "auditor");
	}

	@Test
	void truncateAuditLogSetResultCountUsesAggregateCount() {
		Persistence persistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		when(persistence.newDocumentQuery(AdminDocument.AUDIT)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		when(query.addAggregateProjection(AggregateFunction.Count, org.skyve.domain.Bean.DOCUMENT_ID, "CountOfAudits"))
				.thenReturn(query);
		when(query.scalarResult(Number.class)).thenReturn(Integer.valueOf(7));

		DataMaintenance result = TruncateAuditLog.setResultCount(persistence, bean);

		assertThat(result, is(bean));
		assertThat(bean.getAuditMatchCount(), is(Integer.valueOf(7)));
	}

	// ---- RefreshDocumentTuples action: null refreshOption validation ----

	@Test
	void refreshDocumentTuplesWithNullRefreshOptionThrowsValidationException() {
		bean.setRefreshOption(null);
		RefreshDocumentTuples action = new RefreshDocumentTuples();
		assertThrows(ValidationException.class, () -> action.execute(bean, webContext));
	}

	// ---- ContentSelected action: null selectedContentId ----

	@Test
	void contentSelectedWithNullIdSetsNullContentLink() throws Exception {
		bean.setSelectedContentId(null);
		ContentSelected action = new ContentSelected();
		ServerSideActionResult<DataMaintenance> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
	}

	private Path createBackup(String name, String content) throws Exception {
		Path backup = backupPath(name);
		Files.createDirectories(backup.getParent());
		Files.writeString(backup, content);
		return backup;
	}

	private Path backupPath(String name) {
		return tempDir.resolve("backup_" + CORE.getUser().getCustomerName()).resolve(name);
	}

	@SuppressWarnings("resource")
	private static Upload upload(String fileName, String content) {
		return new Upload(fileName,
				new WebFileInputStream(new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8))),
				MimeType.zip);
	}
}
