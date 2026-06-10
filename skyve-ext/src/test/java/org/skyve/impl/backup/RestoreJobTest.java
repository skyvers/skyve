package org.skyve.impl.backup;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.backup.RestoreOptions.ContentOption;
import org.skyve.impl.backup.RestoreOptions.IndexingOption;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;

@SuppressWarnings({"static-method", "resource"})
class RestoreJobTest {
	private static final String MISSING_CONTENT_ID = "12345678-1234-1234-1234-123456789abc";

	private Class<? extends AbstractContentManager> originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;

	@TempDir
	Path tempDir;

	@AfterEach
	void tearDown() {
		AbstractContentManager.IMPLEMENTATION_CLASS = originalContentManagerClass;
	}

	@Test
	void executeWithWrongBeanTypeLogsDataMaintenanceInstruction() throws Exception {
		RestoreJob job = new RestoreJob();
		job.setBean(mock(Bean.class));

		job.execute();

		assertEquals("Kick off the job with the appropriate options from the Data Maintenance page.", job.getLog().get(0));
	}

	@Test
	void restoreDataBindsCsvValuesForNormalTablesAndSkipsForeignKeys() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		Table table = new Table("demo", "DEMO_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		table.fields.put(Bean.CUSTOMER_NAME, new BackupField(AttributeType.text, Sensitivity.none));
		table.fields.put("name", new BackupField(AttributeType.text, Sensitivity.none));
		table.fields.put("active", new BackupField(AttributeType.bool, Sensitivity.none));
		table.fields.put("quantity", new BackupField(AttributeType.integer, Sensitivity.none));
		table.fields.put("amount", new BackupField(AttributeType.decimal2, Sensitivity.none));
		Files.writeString(tempDir.resolve("demo.csv"),
				"bizId,bizCustomer,name,active,quantity,amount,parent_id\nbean-1,demoCustomer,Alice,true,7,12.34,parent-1\n",
				StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("insert into DEMO_TABLE (bizId,bizCustomer,name,active,quantity,amount) values (?,?,?,?,?,?)"))
				.thenReturn(statement);
		RestoreJob job = new RestoreJob();

		invokeRestoreData(job, tempDir, List.of(table), connection, false, false, ContentOption.error, IndexingOption.none);

		verify(statement).setString(1, "bean-1");
		verify(statement).setString(2, "demoCustomer");
		verify(statement).setString(3, "Alice");
		verify(statement).setBoolean(4, true);
		verify(statement).setInt(5, 7);
		verify(statement).setBigDecimal(6, new BigDecimal("12.34"));
		verify(statement).executeUpdate();
		verify(connection).commit();
		assertEquals("    restored table demo with 1 rows.", job.getLog().get(job.getLog().size() - 1));
	}

	@Test
	void restoreDataSkipsNormalTablesWhenRestoringJoinTablesOnly() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		Table table = new Table("demo", "DEMO_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		Files.writeString(tempDir.resolve("demo.csv"), "bizId\nbean-1\n", StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		RestoreJob job = new RestoreJob();

		invokeRestoreData(job, tempDir, List.of(table), connection, true, false, ContentOption.error, IndexingOption.none);

		verify(connection, never()).prepareStatement(org.mockito.ArgumentMatchers.anyString());
		assertEquals(0, job.getLog().size());
	}

	@Test
	void restoreDataSkipsNormalCustomerTablesDuringExtensionTablePass() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		Table table = new Table("demo", "DEMO_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		table.fields.put(Bean.CUSTOMER_NAME, new BackupField(AttributeType.text, Sensitivity.none));
		Files.writeString(tempDir.resolve("demo.csv"), "bizId,bizCustomer\nbean-1,demoCustomer\n", StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		RestoreJob job = new RestoreJob();

		invokeRestoreData(job, tempDir, List.of(table), connection, false, true, ContentOption.error, IndexingOption.none);

		verify(connection, never()).prepareStatement(org.mockito.ArgumentMatchers.anyString());
		assertEquals(0, job.getLog().size());
	}

	@Test
	void restoreDataBindsExtensionTableWithoutBizCustomerDuringExtensionPass() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		Table table = new Table("demo_ext", "DEMO_EXT");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		table.fields.put("name", new BackupField(AttributeType.text, Sensitivity.none));
		Files.writeString(tempDir.resolve("demo_ext.csv"), "bizId,name\next-1,Extension\n", StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("insert into DEMO_EXT (bizId,name) values (?,?)")).thenReturn(statement);
		RestoreJob job = new RestoreJob();

		invokeRestoreData(job, tempDir, List.of(table), connection, false, true, ContentOption.error, IndexingOption.none);

		verify(statement).setString(1, "ext-1");
		verify(statement).setString(2, "Extension");
		verify(statement).executeUpdate();
		assertEquals("    restored table demo_ext with 1 rows.", job.getLog().get(job.getLog().size() - 1));
	}

	@Test
	void restoreDataLogsAndSkipsMissingBackupCsv() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		Table table = new Table("missing", "MISSING_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		table.fields.put(Bean.CUSTOMER_NAME, new BackupField(AttributeType.text, Sensitivity.none));
		Connection connection = mock(Connection.class);
		RestoreJob job = new RestoreJob();

		invokeRestoreData(job, tempDir, List.of(table), connection, false, false, ContentOption.error, IndexingOption.none);

		verify(connection, never()).prepareStatement(org.mockito.ArgumentMatchers.anyString());
		assertEquals("    restore table missing", job.getLog().get(0));
		assertTrue(job.getLog().get(1).contains("does not exist"));
	}

	@Test
	void restoreDataBindsEmptyCsvValuesAsNulls() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		Table table = new Table("demo", "DEMO_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		table.fields.put(Bean.CUSTOMER_NAME, new BackupField(AttributeType.text, Sensitivity.none));
		table.fields.put("name", new BackupField(AttributeType.text, Sensitivity.none));
		Files.writeString(tempDir.resolve("demo.csv"), "bizId,bizCustomer,name\nbean-1,,\n", StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("insert into DEMO_TABLE (bizId,bizCustomer,name) values (?,?,?)")).thenReturn(statement);
		RestoreJob job = new RestoreJob();

		invokeRestoreData(job, tempDir, List.of(table), connection, false, false, ContentOption.error, IndexingOption.none);

		verify(statement).setString(1, "bean-1");
		verify(statement).setObject(2, null);
		verify(statement).setObject(3, null);
		verify(statement).executeUpdate();
	}

	@Test
	void restoreDataBindsTemporalAndLongValues() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		Table table = new Table("demo", "DEMO_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		table.fields.put(Bean.CUSTOMER_NAME, new BackupField(AttributeType.text, Sensitivity.none));
		table.fields.put("birthDate", new BackupField(AttributeType.date, Sensitivity.none));
		table.fields.put("startTime", new BackupField(AttributeType.time, Sensitivity.none));
		table.fields.put("createdAt", new BackupField(AttributeType.dateTime, Sensitivity.none));
		table.fields.put("updatedAt", new BackupField(AttributeType.timestamp, Sensitivity.none));
		table.fields.put("bigCount", new BackupField(AttributeType.longInteger, Sensitivity.none));
		Files.writeString(tempDir.resolve("demo.csv"),
				"bizId,bizCustomer,birthDate,startTime,createdAt,updatedAt,bigCount\nbean-1,demoCustomer,1000,2000,3000,4000,123456789\n",
				StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("insert into DEMO_TABLE (bizId,bizCustomer,birthDate,startTime,createdAt,updatedAt,bigCount) values (?,?,?,?,?,?,?)"))
				.thenReturn(statement);
		RestoreJob job = new RestoreJob();

		invokeRestoreData(job, tempDir, List.of(table), connection, false, false, ContentOption.error, IndexingOption.none);

		verify(statement).setDate(org.mockito.ArgumentMatchers.eq(3),
				org.mockito.ArgumentMatchers.eq(new java.sql.Date(1000L)),
				org.mockito.ArgumentMatchers.any(java.util.Calendar.class));
		verify(statement).setTime(org.mockito.ArgumentMatchers.eq(4),
				org.mockito.ArgumentMatchers.eq(new java.sql.Time(2000L)),
				org.mockito.ArgumentMatchers.any(java.util.Calendar.class));
		verify(statement).setTimestamp(org.mockito.ArgumentMatchers.eq(5),
				org.mockito.ArgumentMatchers.eq(new java.sql.Timestamp(3000L)),
				org.mockito.ArgumentMatchers.any(java.util.Calendar.class));
		verify(statement).setTimestamp(org.mockito.ArgumentMatchers.eq(6),
				org.mockito.ArgumentMatchers.eq(new java.sql.Timestamp(4000L)),
				org.mockito.ArgumentMatchers.any(java.util.Calendar.class));
		verify(statement).setLong(7, 123456789L);
		verify(statement).executeUpdate();
	}

	@Test
	void restoreDataClearsMissingContentWhenConfiguredToClearOrphans() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		Table table = tableWithContentField();
		Files.writeString(tempDir.resolve("demo.csv"), "bizId,bizCustomer,file\nbean-1,demoCustomer," + MISSING_CONTENT_ID + "\n", StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("insert into DEMO_TABLE (bizId,bizCustomer,file) values (?,?,?)")).thenReturn(statement);
		RestoreJob job = new RestoreJob();

		invokeRestoreData(job, tempDir, List.of(table), connection, false, false, ContentOption.clearOrphanedContentIds, IndexingOption.none);

		verify(statement).setString(3, null);
		assertTrue(job.getLog().stream().anyMatch(line -> line.contains("Setting content to null")));
	}

	@Test
	void restoreDataKeepsMissingContentIdWhenConfiguredToSaveOrphans() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		Table table = tableWithContentField();
		Files.writeString(tempDir.resolve("demo.csv"), "bizId,bizCustomer,file\nbean-1,demoCustomer," + MISSING_CONTENT_ID + "\n", StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("insert into DEMO_TABLE (bizId,bizCustomer,file) values (?,?,?)")).thenReturn(statement);
		RestoreJob job = new RestoreJob();

		invokeRestoreData(job, tempDir, List.of(table), connection, false, false, ContentOption.saveOrphanedContentIds, IndexingOption.none);

		verify(statement).setString(3, MISSING_CONTENT_ID);
		assertTrue(job.getLog().stream().anyMatch(line -> line.contains("Setting content ID regardless")));
	}

	@Test
	void restoreDataThrowsWhenMissingContentIsConfiguredAsError() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		Table table = tableWithContentField();
		Files.writeString(tempDir.resolve("demo.csv"), "bizId,bizCustomer,file\nbean-1,demoCustomer," + MISSING_CONTENT_ID + "\n", StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("insert into DEMO_TABLE (bizId,bizCustomer,file) values (?,?,?)")).thenReturn(statement);
		RestoreJob job = new RestoreJob();

		List<Table> tables1 = List.of(table);
		DomainException e = assertThrows(DomainException.class,
				() -> invokeRestoreData(job, tempDir, tables1, connection, false, false, ContentOption.error, IndexingOption.none));

		assertTrue(e.getMessage().contains("Could not find file associated with " + MISSING_CONTENT_ID));
		assertTrue(job.getLog().stream().anyMatch(line -> line.contains("CAUSED BY:- insert into DEMO_TABLE")));
	}

	@Test
	void restoreDataPutsExistingContentAndBindsRestoredContentId() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = CapturingContentManager.class;
		CapturingContentManager.reset();
		String contentId = "12345678-1234-1234-1234-123456789abd";
		Table table = tableWithContentField();
		Files.writeString(tempDir.resolve("demo.csv"), "bizId,bizCustomer,file\nbean-1,demoCustomer," + contentId + "\n", StandardCharsets.UTF_8);
		AttachmentContent attachment = new AttachmentContent("demoCustomer", "sales", "Invoice", null, "admin", "bean-1", "file")
				.attachment("invoice.txt", "text/plain", "invoice".getBytes(StandardCharsets.UTF_8));
		attachment.setContentId(contentId);
		attachment.setLastModified(new java.util.Date(1000L));
		StringBuilder contentRoot = new StringBuilder(tempDir.toAbsolutePath().toString())
				.append('/')
				.append(ContentManager.FILE_STORE_NAME)
				.append('/');
		AbstractContentManager.writeContentFiles(contentRoot,
				attachment,
				new ByteArrayInputStream("invoice".getBytes(StandardCharsets.UTF_8)),
				true);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("insert into DEMO_TABLE (bizId,bizCustomer,file) values (?,?,?)")).thenReturn(statement);
		RestoreJob job = new RestoreJob();

		invokeRestoreData(job, tempDir, List.of(table), connection, false, false, ContentOption.error, IndexingOption.content);

		assertNotNull(CapturingContentManager.lastAttachment);
		assertEquals(contentId, CapturingContentManager.lastAttachment.getContentId());
		assertTrue(CapturingContentManager.lastIndex);
		verify(statement).setString(3, contentId);
		verify(statement).executeUpdate();
	}

	@Test
	void restoreDataReturnsWithoutWritingRowsWhenCancelled() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		Table table = new Table("demo", "DEMO_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		table.fields.put(Bean.CUSTOMER_NAME, new BackupField(AttributeType.text, Sensitivity.none));
		Files.writeString(tempDir.resolve("demo.csv"), "bizId,bizCustomer\nbean-1,demoCustomer\n", StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("insert into DEMO_TABLE (bizId,bizCustomer) values (?,?)")).thenReturn(statement);

		invokeRestoreData(new CancelledRestoreJob(), tempDir, List.of(table), connection, false, false, ContentOption.error, IndexingOption.none);

		verify(statement, never()).clearParameters();
		verify(statement, never()).executeUpdate();
		verify(connection, never()).commit();
	}

	@Test
	void restoreDataBindsJoinTableForeignKeysAndOrdinal() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		JoinTable table = new JoinTable("demo_join", "DEMO_JOIN", "demo", "DEMO_TABLE", true);
		Files.writeString(tempDir.resolve("demo_join.csv"),
				"owner_id,element_id,bizOrdinal\nowner-1,element-1,3\n",
				StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("insert into DEMO_JOIN (owner_id,element_id,bizOrdinal) values (?,?,?)"))
				.thenReturn(statement);
		RestoreJob job = new RestoreJob();

		invokeRestoreData(job, tempDir, List.of(table), connection, true, false, ContentOption.error, IndexingOption.none);

		verify(statement).setString(1, "owner-1");
		verify(statement).setString(2, "element-1");
		verify(statement).setInt(3, 3);
		verify(statement).executeUpdate();
		verify(connection).commit();
		assertEquals("    restored table demo_join with 1 rows.", job.getLog().get(job.getLog().size() - 1));
	}

	@Test
	void restoreDataLogsSqlAndValuesWhenColumnTypeIsUnknown() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		Table table = new Table("demo", "DEMO_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		table.fields.put(Bean.CUSTOMER_NAME, new BackupField(AttributeType.text, Sensitivity.none));
		Files.writeString(tempDir.resolve("demo.csv"),
				"bizId,bizCustomer,mystery\nbean-1,demoCustomer,value\n",
				StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("insert into DEMO_TABLE (bizId,bizCustomer,mystery) values (?,?,?)")).thenReturn(statement);
		RestoreJob job = new RestoreJob();

		List<Table> tables2 = List.of(table);
		IllegalStateException e = assertThrows(IllegalStateException.class,
				() -> invokeRestoreData(job, tempDir, tables2, connection, false, false, ContentOption.error, IndexingOption.none));

		assertTrue(e.getMessage().contains("unknown attribute type"));
		assertTrue(job.getLog().stream().anyMatch(line -> line.contains("CAUSED BY:- insert into DEMO_TABLE")));
		assertTrue(job.getLog().stream().anyMatch(line -> line.contains("mystery=value")));
	}

	@Test
	void restoreForeignKeysUpdatesAssociationColumnsByBizId() throws Exception {
		Table table = new Table("demo", "DEMO_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		table.fields.put("parent_id", new BackupField(AttributeType.association, Sensitivity.none));
		Files.writeString(tempDir.resolve("demo.csv"),
				"bizId,parent_id,name\nbean-1,parent-1,Alice\nbean-2,,Bob\n",
				StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("update DEMO_TABLE set parent_id = ? where bizId = ?")).thenReturn(statement);
		RestoreJob job = new RestoreJob();

		invokeRestoreForeignKeys(job, tempDir, List.of(table), connection);

		verify(statement).setString(1, "parent-1");
		verify(statement).setObject(1, null);
		verify(statement).setString(2, "bean-1");
		verify(statement).setString(2, "bean-2");
		verify(statement, org.mockito.Mockito.times(2)).executeUpdate();
		verify(connection).commit();
		assertEquals("    restored foreign keys for table demo with 2 rows.", job.getLog().get(job.getLog().size() - 1));
	}

	@Test
	void restoreForeignKeysSkipsJoinTables() throws Exception {
		JoinTable table = new JoinTable("demo_join", "DEMO_JOIN", "demo", "DEMO_TABLE", false);
		Files.writeString(tempDir.resolve("demo_join.csv"), "owner_id,element_id\nowner-1,element-1\n", StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		RestoreJob job = new RestoreJob();

		invokeRestoreForeignKeys(job, tempDir, List.of(table), connection);

		verify(connection, never()).prepareStatement(org.mockito.ArgumentMatchers.anyString());
		assertEquals(0, job.getLog().size());
	}

	@Test
	void restoreForeignKeysLogsAndSkipsMissingBackupCsv() throws Exception {
		Table table = new Table("missing", "MISSING_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		Connection connection = mock(Connection.class);
		RestoreJob job = new RestoreJob();

		invokeRestoreForeignKeys(job, tempDir, List.of(table), connection);

		verify(connection, never()).prepareStatement(org.mockito.ArgumentMatchers.anyString());
		assertEquals("    restore foreign keys for table missing", job.getLog().get(0));
		assertTrue(job.getLog().get(1).contains("does not exist"));
	}

	@Test
	void restoreForeignKeysHandlesMultipleAssociationColumns() throws Exception {
		Table table = new Table("demo", "DEMO_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		table.fields.put("parent_id", new BackupField(AttributeType.association, Sensitivity.none));
		table.fields.put("owner_id", new BackupField(AttributeType.association, Sensitivity.none));
		Files.writeString(tempDir.resolve("demo.csv"),
				"bizId,parent_id,owner_id\nbean-1,parent-1,owner-1\n",
				StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("update DEMO_TABLE set parent_id = ?, owner_id = ? where bizId = ?")).thenReturn(statement);
		RestoreJob job = new RestoreJob();

		invokeRestoreForeignKeys(job, tempDir, List.of(table), connection);

		verify(statement).setString(1, "parent-1");
		verify(statement).setString(2, "owner-1");
		verify(statement).setString(3, "bean-1");
		verify(statement).executeUpdate();
	}

	@Test
	void restoreForeignKeysDoesNotPrepareStatementWhenCsvHasNoForeignKeys() throws Exception {
		Table table = new Table("demo", "DEMO_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		table.fields.put("name", new BackupField(AttributeType.text, Sensitivity.none));
		Files.writeString(tempDir.resolve("demo.csv"), "bizId,name\nbean-1,Alice\n", StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		RestoreJob job = new RestoreJob();

		invokeRestoreForeignKeys(job, tempDir, List.of(table), connection);

		verify(connection, never()).prepareStatement(org.mockito.ArgumentMatchers.anyString());
		assertEquals("    restored foreign keys for table demo with 0 rows.", job.getLog().get(job.getLog().size() - 1));
	}

	@Test
	void restoreForeignKeysReturnsWithoutWritingRowsWhenCancelled() throws Exception {
		Table table = new Table("demo", "DEMO_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		table.fields.put("parent_id", new BackupField(AttributeType.association, Sensitivity.none));
		Files.writeString(tempDir.resolve("demo.csv"), "bizId,parent_id\nbean-1,parent-1\n", StandardCharsets.UTF_8);
		Connection connection = mock(Connection.class);
		PreparedStatement statement = mock(PreparedStatement.class);
		when(connection.prepareStatement("update DEMO_TABLE set parent_id = ? where bizId = ?")).thenReturn(statement);

		invokeRestoreForeignKeys(new CancelledRestoreJob(), tempDir, List.of(table), connection);

		verify(statement, never()).clearParameters();
		verify(statement, never()).executeUpdate();
		verify(connection, never()).commit();
	}

	private static void invokeRestoreData(RestoreJob job,
											Path backupDirectory,
											List<Table> tables,
											Connection connection,
											boolean joinTables,
											boolean extensionTables,
											ContentOption contentOption,
											IndexingOption indexingOption)
	throws Exception {
		Method method = RestoreJob.class.getDeclaredMethod("restoreData",
				java.io.File.class,
				java.util.Collection.class,
				Connection.class,
				boolean.class,
				boolean.class,
				ContentOption.class,
				IndexingOption.class);
		method.setAccessible(true);
		invokeAndUnwrap(method, job, backupDirectory.toFile(), tables, connection, Boolean.valueOf(joinTables), Boolean.valueOf(extensionTables), contentOption, indexingOption);
	}

	private static Table tableWithContentField() {
		Table table = new Table("demo", "DEMO_TABLE");
		table.fields.put(Bean.DOCUMENT_ID, new BackupField(AttributeType.id, Sensitivity.none));
		table.fields.put(Bean.CUSTOMER_NAME, new BackupField(AttributeType.text, Sensitivity.none));
		table.fields.put("file", new BackupField(AttributeType.content, Sensitivity.none));
		return table;
	}

	private static void invokeRestoreForeignKeys(RestoreJob job, Path backupDirectory, List<Table> tables, Connection connection)
	throws Exception {
		Method method = RestoreJob.class.getDeclaredMethod("restoreForeignKeys",
				java.io.File.class,
				java.util.Collection.class,
				Connection.class);
		method.setAccessible(true);
		invokeAndUnwrap(method, job, backupDirectory.toFile(), tables, connection);
	}

	private static Object invokeAndUnwrap(Method method, Object target, Object... args) throws Exception {
		try {
			return method.invoke(target, args);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			if (cause instanceof Error error) {
				throw error;
			}
			throw new RuntimeException(cause);
		}
	}

	private static class CancelledRestoreJob extends RestoreJob {
		@Override
		public boolean isCancelled() {
			return true;
		}
	}

	public static class CapturingContentManager extends NoOpContentManager {
		static AttachmentContent lastAttachment;
		static boolean lastIndex;

		static void reset() {
			lastAttachment = null;
			lastIndex = false;
		}

		@Override
		public void put(AttachmentContent content, boolean index) {
			lastAttachment = content;
			lastIndex = index;
		}
	}
}
