package org.skyve.impl.backup;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Collection;
import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.job.CancellableJob;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Generated;

/**
 * Reindexes all binary attachment content (uploaded files) in the content
 * search engine after a restore or configuration change.
 */
public class ReindexAttachmentsJob extends CancellableJob {
	@Override
	public void execute() throws Exception {
		String customerName = getCustomerName();
		List<String> log = getLog();
		String trace;
		
		// truncate the attachment content ready to reindex
		try (ContentManager cm = newContentManager()) {
			trace = "Truncate Attachments";
			log.add(trace);
			LOGGER.info(trace);
			cm.truncateAttachmentIndexing(customerName);
		}

		try (Connection connection = getDataStoreConnection()) {
			connection.setAutoCommit(false);

			try (ContentManager cm = newContentManager()) {
				if (! isAbstractContentManager(cm)) {
					return;
				}
				AbstractContentManager acm = (AbstractContentManager) cm;
				Collection<Table> tables = getTables();
				int i = 0;
				int l = tables.size();
				for (Table table : tables) {
					i++;
					if (! hasContent(table)) {
						trace = "Skipping table " + table.persistentIdentifier;
						getLog().add(trace);
						LOGGER.info(trace);
                		continue;
                	}

                	StringBuilder sql = new StringBuilder(128);
					try (Statement statement = connection.createStatement()) {
						sql.append("select * from ").append(table.persistentIdentifier);
						secureSQL(sql, table, customerName);
						statement.execute(sql.toString());
						try (ResultSet resultSet = statement.getResultSet()) {
							trace = "Reindexing content for " + table.persistentIdentifier;
							log.add(trace);
							LOGGER.info(trace);

							while (resultSet.next()) {
								if (isCancelled()) {
									return;
								}
								for (String name : table.fields.keySet()) {
									AttributeType attributeType = table.fields.get(name).getAttributeType();
									if (AttributeType.content.equals(attributeType) ||
											AttributeType.image.equals(attributeType)) {
										String stringValue = resultSet.getString(name);
										if (! resultSet.wasNull()) {
											AttachmentContent content;
											try {
												content = cm.getAttachment(stringValue);
												if (content == null) {
													trace = String.format("Error reindexing content %s for field name %s for table %s - content does not exist",
																			stringValue, name, table.persistentIdentifier);
													log.add(trace);
													LOGGER.error(trace);
												}
												else {
													IndexType indexType = table.indexes.get(name);
													boolean index = ((indexType == null) || 
																		IndexType.textual.equals(indexType) ||
																		IndexType.both.equals(indexType));
													acm.reindex(content, index);
												}
											}
											catch (Exception e) {
												trace = String.format("Error reindexing content %s for field name %s for table %s - caused by %s",
																		stringValue, name, table.persistentIdentifier, e.getLocalizedMessage());
												log.add(trace);
												LOGGER.error(trace, e);
											}
										}
									}
								}
							}
						}
					}
					setPercentComplete((int) ((((float) i) / ((float) l)) * 100f));
				}
			}
		}
		trace = "Reindexing content complete";
		log.add(trace);
		LOGGER.info(trace);
		setPercentComplete(100);
	}

	/**
	 * Returns the customer name used to scope attachment index operations.
	 *
	 * @return the active customer name
	 */
	@SuppressWarnings("static-method")
	@Generated("test seam")
	protected String getCustomerName() {
		return CORE.getUser().getCustomerName();
	}

	/**
	 * Creates a content manager for index truncation and reindex operations.
	 *
	 * <p>Side effects: may open underlying storage resources.
	 *
	 * @return a new content manager instance
	 */
	@SuppressWarnings({"static-method", "resource"})
	@Generated("test seam")
	protected ContentManager newContentManager() {
		return EXT.newContentManager();
	}

	/**
	 * Returns a datastore connection used to stream rows for reindexing.
	 *
	 * @return an open JDBC connection
	 */
	@SuppressWarnings({"static-method", "resource"})
	@Generated("test seam")
	protected Connection getDataStoreConnection() {
		return EXT.getDataStoreConnection();
	}

	/**
	 * Loads table metadata used to identify attachment-bearing columns.
	 *
	 * @return backup table metadata for the current environment
	 * @throws Exception if metadata lookup fails
	 */
	@SuppressWarnings("static-method")
	@Generated("test seam")
	protected Collection<Table> getTables() throws Exception {
		return BackupUtil.getTables();
	}

	/**
	 * Appends customer scoping clauses to a SQL statement for a table scan.
	 *
	 * @param sql the SQL statement under construction
	 * @param table the table metadata being queried
	 * @param customerName the customer scope to apply
	 */
	@SuppressWarnings("static-method")
	@Generated("test seam")
	protected void secureSQL(StringBuilder sql, Table table, String customerName) {
		BackupUtil.secureSQL(sql, table, customerName);
	}

	/**
	 * Checks whether the content manager supports direct reindex operations.
	 *
	 * @param cm the content manager instance to inspect
	 * @return {@code true} when the manager is an {@link AbstractContentManager}
	 */
	@SuppressWarnings("static-method")
	@Generated("test seam")
	protected boolean isAbstractContentManager(ContentManager cm) {
		return cm instanceof AbstractContentManager;
	}
	
	/**
	 * Determines whether a table contains any content or image attributes.
	 *
	 * @param table the table metadata to inspect
	 * @return {@code true} when the table has at least one content-bearing field
	 */
	private static boolean hasContent(Table table) {
		for (String name : table.fields.keySet()) {
			AttributeType attributeType = table.fields.get(name).getAttributeType();
			if (AttributeType.content.equals(attributeType) ||
					AttributeType.image.equals(attributeType)) {
				return true;
			}
		}

		return false;
	}
}
