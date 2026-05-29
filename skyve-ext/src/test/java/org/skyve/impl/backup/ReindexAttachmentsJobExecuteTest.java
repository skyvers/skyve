package org.skyve.impl.backup;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.List;

import org.junit.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.metadata.model.Attribute.AttributeType;

@SuppressWarnings({"static-method", "boxing", "resource", "hiding"})
public class ReindexAttachmentsJobExecuteTest {
	@Test
	public void executeReturnsEarlyWhenContentManagerCannotReindex() throws Exception {
		Connection connection = mock(Connection.class);
		ContentManager truncateCm = mock(ContentManager.class);
		ContentManager scanCm = mock(ContentManager.class);

		TestableReindexAttachmentsJob job = new TestableReindexAttachmentsJob("demo", connection, List.of(), truncateCm, scanCm);
		job.abstractContentManager = false;

		job.execute();

		verify(truncateCm).truncateAttachmentIndexing("demo");
		verify(connection).setAutoCommit(false);
		verify(connection, never()).createStatement();
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("Truncate Attachments")));
	}

	@Test
	public void executeSkipsTableWithoutContentAndCompletes() throws Exception {
		Connection connection = mock(Connection.class);
		AbstractContentManager truncateCm = mock(AbstractContentManager.class);
		AbstractContentManager scanCm = mock(AbstractContentManager.class);
		Table table = new Table("agnostic", "ADM_TEST");
		table.fields.put("name", new BackupField(AttributeType.text, org.skyve.metadata.model.Attribute.Sensitivity.none));

		TestableReindexAttachmentsJob job = new TestableReindexAttachmentsJob("demo", connection, List.of(table), truncateCm, scanCm);
		job.execute();

		verify(connection).setAutoCommit(false);
		verify(connection, never()).createStatement();
		assertEquals(100, job.getPercentComplete());
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("Skipping table ADM_TEST")));
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("Reindexing content complete")));
	}

	@Test
	public void executeReindexesAttachmentWhenLookupSucceeds() throws Exception {
		Connection connection = mock(Connection.class);
		Statement statement = mock(Statement.class);
		ResultSet resultSet = mock(ResultSet.class);
		AbstractContentManager truncateCm = mock(AbstractContentManager.class);
		AbstractContentManager scanCm = mock(AbstractContentManager.class);
		AttachmentContent attachment = mock(AttachmentContent.class);
		Table table = new Table("agnostic", "ADM_CONTENT");
		table.fields.put("file", new BackupField(AttributeType.content, org.skyve.metadata.model.Attribute.Sensitivity.none));
		table.indexes.put("file", IndexType.both);

		when(connection.createStatement()).thenReturn(statement);
		when(statement.getResultSet()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(Boolean.TRUE, Boolean.FALSE);
		when(resultSet.getString("file")).thenReturn("cid-1");
		when(resultSet.wasNull()).thenReturn(Boolean.FALSE);
		when(scanCm.getAttachment("cid-1")).thenReturn(attachment);

		TestableReindexAttachmentsJob job = new TestableReindexAttachmentsJob("demo", connection, List.of(table), truncateCm, scanCm);
		job.execute();

		verify(scanCm).reindex(attachment, true);
		assertEquals(100, job.getPercentComplete());
	}

	@Test
	public void executeProcessesMultipleContentTablesAndSkipsMixedNonContentTable() throws Exception {
		Connection connection = mock(Connection.class);
		Statement firstStatement = mock(Statement.class);
		Statement secondStatement = mock(Statement.class);
		ResultSet firstResultSet = mock(ResultSet.class);
		ResultSet secondResultSet = mock(ResultSet.class);
		AbstractContentManager truncateCm = mock(AbstractContentManager.class);
		AbstractContentManager scanCm = mock(AbstractContentManager.class);
		AttachmentContent firstAttachment = mock(AttachmentContent.class);
		AttachmentContent secondAttachment = mock(AttachmentContent.class);
		Table firstTable = new Table("agnostic", "ADM_FIRST");
		firstTable.fields.put("file", new BackupField(AttributeType.content, org.skyve.metadata.model.Attribute.Sensitivity.none));
		firstTable.indexes.put("file", IndexType.both);
		Table skippedTable = new Table("agnostic", "ADM_SKIPPED");
		skippedTable.fields.put("name", new BackupField(AttributeType.text, org.skyve.metadata.model.Attribute.Sensitivity.none));
		Table secondTable = new Table("agnostic", "ADM_SECOND");
		secondTable.fields.put("image", new BackupField(AttributeType.image, org.skyve.metadata.model.Attribute.Sensitivity.none));
		secondTable.indexes.put("image", IndexType.textual);

		when(connection.createStatement()).thenReturn(firstStatement, secondStatement);
		when(firstStatement.getResultSet()).thenReturn(firstResultSet);
		when(secondStatement.getResultSet()).thenReturn(secondResultSet);
		when(firstResultSet.next()).thenReturn(Boolean.TRUE, Boolean.FALSE);
		when(secondResultSet.next()).thenReturn(Boolean.TRUE, Boolean.FALSE);
		when(firstResultSet.getString("file")).thenReturn("cid-6");
		when(secondResultSet.getString("image")).thenReturn("cid-7");
		when(firstResultSet.wasNull()).thenReturn(Boolean.FALSE);
		when(secondResultSet.wasNull()).thenReturn(Boolean.FALSE);
		when(scanCm.getAttachment("cid-6")).thenReturn(firstAttachment);
		when(scanCm.getAttachment("cid-7")).thenReturn(secondAttachment);

		TestableReindexAttachmentsJob job = new TestableReindexAttachmentsJob("demo", connection, List.of(firstTable, skippedTable, secondTable), truncateCm, scanCm);
		job.execute();

		verify(scanCm).reindex(firstAttachment, true);
		verify(scanCm).reindex(secondAttachment, true);
		assertEquals(100, job.getPercentComplete());
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("Skipping table ADM_SKIPPED")));
	}

	@Test
	public void executeUsesFalseIndexWhenFieldIndexTypeIsNone() throws Exception {
		Connection connection = mock(Connection.class);
		Statement statement = mock(Statement.class);
		ResultSet resultSet = mock(ResultSet.class);
		AbstractContentManager truncateCm = mock(AbstractContentManager.class);
		AbstractContentManager scanCm = mock(AbstractContentManager.class);
		AttachmentContent attachment = mock(AttachmentContent.class);
		Table table = new Table("agnostic", "ADM_CONTENT");
		table.fields.put("file", new BackupField(AttributeType.content, org.skyve.metadata.model.Attribute.Sensitivity.none));
		table.indexes.put("file", IndexType.none);

		when(connection.createStatement()).thenReturn(statement);
		when(statement.getResultSet()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(Boolean.TRUE, Boolean.FALSE);
		when(resultSet.getString("file")).thenReturn("cid-2");
		when(resultSet.wasNull()).thenReturn(Boolean.FALSE);
		when(scanCm.getAttachment("cid-2")).thenReturn(attachment);

		TestableReindexAttachmentsJob job = new TestableReindexAttachmentsJob("demo", connection, List.of(table), truncateCm, scanCm);
		job.execute();

		verify(scanCm).reindex(attachment, false);
	}

	@Test
	public void executeLogsAttachmentErrorsAndContinues() throws Exception {
		Connection connection = mock(Connection.class);
		Statement statement = mock(Statement.class);
		ResultSet resultSet = mock(ResultSet.class);
		AbstractContentManager truncateCm = mock(AbstractContentManager.class);
		AbstractContentManager scanCm = mock(AbstractContentManager.class);
		Table table = new Table("agnostic", "ADM_CONTENT");
		table.fields.put("file", new BackupField(AttributeType.content, org.skyve.metadata.model.Attribute.Sensitivity.none));

		when(connection.createStatement()).thenReturn(statement);
		when(statement.getResultSet()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(Boolean.TRUE, Boolean.FALSE);
		when(resultSet.getString("file")).thenReturn("cid-3");
		when(resultSet.wasNull()).thenReturn(Boolean.FALSE);
		when(scanCm.getAttachment("cid-3")).thenThrow(new IllegalStateException("missing"));

		TestableReindexAttachmentsJob job = new TestableReindexAttachmentsJob("demo", connection, List.of(table), truncateCm, scanCm);
		job.execute();

		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("Error reindexing content cid-3")));
	}

	@Test
	public void executeLogsWhenAttachmentDoesNotExist() throws Exception {
		Connection connection = mock(Connection.class);
		Statement statement = mock(Statement.class);
		ResultSet resultSet = mock(ResultSet.class);
		AbstractContentManager truncateCm = mock(AbstractContentManager.class);
		AbstractContentManager scanCm = mock(AbstractContentManager.class);
		Table table = new Table("agnostic", "ADM_CONTENT");
		table.fields.put("image", new BackupField(AttributeType.image, org.skyve.metadata.model.Attribute.Sensitivity.none));

		when(connection.createStatement()).thenReturn(statement);
		when(statement.getResultSet()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(Boolean.TRUE, Boolean.FALSE);
		when(resultSet.getString("image")).thenReturn("cid-4");
		when(resultSet.wasNull()).thenReturn(Boolean.FALSE);
		when(scanCm.getAttachment("cid-4")).thenReturn(null);

		TestableReindexAttachmentsJob job = new TestableReindexAttachmentsJob("demo", connection, List.of(table), truncateCm, scanCm);
		job.execute();

		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("content does not exist")));
	}

	@Test
	public void executeReturnsEarlyWhenCancelled() throws Exception {
		Connection connection = mock(Connection.class);
		Statement statement = mock(Statement.class);
		ResultSet resultSet = mock(ResultSet.class);
		AbstractContentManager truncateCm = mock(AbstractContentManager.class);
		AbstractContentManager scanCm = mock(AbstractContentManager.class);
		Table table = new Table("agnostic", "ADM_CONTENT");
		table.fields.put("file", new BackupField(AttributeType.content, org.skyve.metadata.model.Attribute.Sensitivity.none));

		when(connection.createStatement()).thenReturn(statement);
		when(statement.getResultSet()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(Boolean.TRUE);

		TestableReindexAttachmentsJob job = new TestableReindexAttachmentsJob("demo", connection, List.of(table), truncateCm, scanCm);
		job.cancelled = true;
		job.execute();

		verify(scanCm, never()).getAttachment("file");
	}

	@Test
	public void executeSkipsNonContentFieldWithinMixedTable() throws Exception {
		Connection connection = mock(Connection.class);
		Statement statement = mock(Statement.class);
		ResultSet resultSet = mock(ResultSet.class);
		AbstractContentManager truncateCm = mock(AbstractContentManager.class);
		AbstractContentManager scanCm = mock(AbstractContentManager.class);
		AttachmentContent attachment = mock(AttachmentContent.class);
		Table table = new Table("agnostic", "ADM_CONTENT");
		table.fields.put("file", new BackupField(AttributeType.content, org.skyve.metadata.model.Attribute.Sensitivity.none));
		table.fields.put("name", new BackupField(AttributeType.text, org.skyve.metadata.model.Attribute.Sensitivity.none));

		when(connection.createStatement()).thenReturn(statement);
		when(statement.getResultSet()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(Boolean.TRUE, Boolean.FALSE);
		when(resultSet.getString("file")).thenReturn("cid-5");
		when(resultSet.wasNull()).thenReturn(Boolean.FALSE);
		when(scanCm.getAttachment("cid-5")).thenReturn(attachment);

		TestableReindexAttachmentsJob job = new TestableReindexAttachmentsJob("demo", connection, List.of(table), truncateCm, scanCm);
		job.execute();

		verify(scanCm).getAttachment("cid-5");
	}

	@Test
	public void executeSkipsNullResultSetValues() throws Exception {
		Connection connection = mock(Connection.class);
		Statement statement = mock(Statement.class);
		ResultSet resultSet = mock(ResultSet.class);
		AbstractContentManager truncateCm = mock(AbstractContentManager.class);
		AbstractContentManager scanCm = mock(AbstractContentManager.class);
		Table table = new Table("agnostic", "ADM_CONTENT");
		table.fields.put("file", new BackupField(AttributeType.content, org.skyve.metadata.model.Attribute.Sensitivity.none));

		when(connection.createStatement()).thenReturn(statement);
		when(statement.getResultSet()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(Boolean.TRUE, Boolean.FALSE);
		when(resultSet.getString("file")).thenReturn("cid-null");
		when(resultSet.wasNull()).thenReturn(Boolean.TRUE);

		TestableReindexAttachmentsJob job = new TestableReindexAttachmentsJob("demo", connection, List.of(table), truncateCm, scanCm);
		job.execute();

		verify(scanCm, never()).getAttachment("cid-null");
	}

	@Test
	public void executeIndexesWhenIndexTypeTextual() throws Exception {
		Connection connection = mock(Connection.class);
		Statement statement = mock(Statement.class);
		ResultSet resultSet = mock(ResultSet.class);
		AbstractContentManager truncateCm = mock(AbstractContentManager.class);
		AbstractContentManager scanCm = mock(AbstractContentManager.class);
		AttachmentContent attachment = mock(AttachmentContent.class);
		Table table = new Table("agnostic", "ADM_CONTENT");
		table.fields.put("file", new BackupField(AttributeType.content, org.skyve.metadata.model.Attribute.Sensitivity.none));
		table.indexes.put("file", IndexType.textual);

		when(connection.createStatement()).thenReturn(statement);
		when(statement.getResultSet()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(Boolean.TRUE, Boolean.FALSE);
		when(resultSet.getString("file")).thenReturn("cid-textual");
		when(resultSet.wasNull()).thenReturn(Boolean.FALSE);
		when(scanCm.getAttachment("cid-textual")).thenReturn(attachment);

		TestableReindexAttachmentsJob job = new TestableReindexAttachmentsJob("demo", connection, List.of(table), truncateCm, scanCm);
		job.execute();

		verify(scanCm).reindex(attachment, true);
	}

	private static final class TestableReindexAttachmentsJob extends ReindexAttachmentsJob {
		private final String customerName;
		private final Connection connection;
		private final Collection<Table> tables;
		private final ArrayDeque<ContentManager> contentManagers = new ArrayDeque<>();
		private boolean abstractContentManager = true;
		private boolean cancelled = false;

		private TestableReindexAttachmentsJob(String customerName,
										Connection connection,
										Collection<Table> tables,
										ContentManager... cms) {
			this.customerName = customerName;
			this.connection = connection;
			this.tables = tables;
			for (ContentManager cm : cms) {
				contentManagers.add(cm);
			}
		}

		@Override
		protected String getCustomerName() {
			return customerName;
		}

		@Override
		protected ContentManager newContentManager() {
			return contentManagers.removeFirst();
		}

		@Override
		protected Connection getDataStoreConnection() {
			return connection;
		}

		@Override
		protected Collection<Table> getTables() {
			return tables;
		}

		@Override
		protected void secureSQL(StringBuilder sql, Table table, String customerName) {
			// no-op for deterministic unit testing
		}

		@Override
		protected boolean isAbstractContentManager(ContentManager cm) {
			return abstractContentManager;
		}

		@Override
		public boolean isCancelled() {
			return cancelled;
		}
	}
}
