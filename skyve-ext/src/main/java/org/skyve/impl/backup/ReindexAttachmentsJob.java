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

public class ReindexAttachmentsJob extends CancellableJob {
	@Override
	@SuppressWarnings("resource")
	public void execute() throws Exception {
		String customerName = CORE.getUser().getCustomerName();
		List<String> log = getLog();
		String trace;
		
		// truncate the attachment content ready to reindex
		try (ContentManager cm = EXT.newContentManager()) {
			trace = "Truncate Attachments";
			log.add(trace);
			LOGGER.info(trace);
			cm.truncateAttachments(customerName);
		}

		try (Connection connection = EXT.getDataStoreConnection()) {
			connection.setAutoCommit(false);

			try (ContentManager cm = EXT.newContentManager()) {
				AbstractContentManager acm;
				if (cm instanceof AbstractContentManager) {
					acm = (AbstractContentManager) cm;
				}
				else {
					return;
				}
				Collection<Table> tables = BackupUtil.getTables();
				float i = 0, l = tables.size();
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
						BackupUtil.secureSQL(sql, table, customerName);
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
												LOGGER.error(trace);
												e.printStackTrace();
											}
										}
									}
								}
							}
						}
					}
					setPercentComplete((int) (i / l * 100f));
				}
			}
		}
		trace = "Reindexing content complete";
		log.add(trace);
		LOGGER.info(trace);
		setPercentComplete(100);
	}
	
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
