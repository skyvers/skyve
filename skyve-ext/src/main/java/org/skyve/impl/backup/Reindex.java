package org.skyve.impl.backup;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.content.elastic.ElasticContentManager;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;

public class Reindex {
	private Reindex() {
		// nothing to see here
	}

	@SuppressWarnings("resource")
	public static void attachments() throws Exception {
		String customerName = CORE.getUser().getCustomerName();
		
		try (Connection connection = EXT.getDataStoreConnection()) {
			connection.setAutoCommit(false);

			try (ContentManager cm = EXT.newContentManager()) {
				ElasticContentManager ecm;
				if (cm instanceof ElasticContentManager) {
					ecm = (ElasticContentManager) cm;
				}
				else {
					return;
				}
				for (Table table : BackupUtil.getTables()) {
                	if (! hasContent(table)) {
                		continue;
                	}

                	StringBuilder sql = new StringBuilder(128);
					try (Statement statement = connection.createStatement()) {
						sql.append("select * from ").append(table.name);
						BackupUtil.secureSQL(sql, table, customerName);
						statement.execute(sql.toString());
						try (ResultSet resultSet = statement.getResultSet()) {
							UtilImpl.LOGGER.info("Reindexing content for " + table.name);

							while (resultSet.next()) {
								for (String name : table.fields.keySet()) {
									AttributeType attributeType = table.fields.get(name);
									if (AttributeType.content.equals(attributeType)) {
										String stringValue = resultSet.getString(name);
										if (! resultSet.wasNull()) {
											AttachmentContent content;
											try {
												content = cm.get(stringValue);
												if (content == null) {
													UtilImpl.LOGGER.severe(String.format("Error reindexing content %s for field name %s for table %s - content does not exist",
																							stringValue, name, table.name));
												}
												else {
													IndexType indexType = table.indexes.get(name);
													boolean index = ((indexType == null) || 
																		IndexType.textual.equals(indexType) ||
																		IndexType.both.equals(indexType));
													ecm.reindex(content, index);
												}
											}
											catch (Exception e) {
												UtilImpl.LOGGER.severe(String.format("Error reindexing content %s for field name %s for table %s - caused by %s",
																						stringValue, name, table.name, e.getLocalizedMessage()));
												e.printStackTrace();
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	public static void beans() throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		Customer customer = persistence.getUser().getCustomer();

		// truncate the bean content ready to reindex
		try (ContentManager cm = EXT.newContentManager()) {
			UtilImpl.LOGGER.info("Truncate Beans");
			cm.truncateBeans(customer.getName());
		}
		
		// reindex
		for (Module module : customer.getModules()) {
			String moduleName = module.getName();

			Map<String, DocumentRef> refs = module.getDocumentRefs();
			for (String documentName : refs.keySet()) {
				DocumentRef ref = refs.get(documentName);
				// is the document defined in this module?
				if (moduleName.equals(ref.getOwningModuleName())) {
					Document document = module.getDocument(customer, documentName);
					Persistent persistent = document.getPersistent(); 
					if ((persistent != null) && (persistent.getName() != null)) { // is persistent
						try {
							persistence.begin();
							try (ContentManager cm = EXT.newContentManager()) {
								// Don't check if a document has indexable fields as we
								// may need to have nodes deleted
								// (i.e. a document field used to be indexed but now is not)
								UtilImpl.LOGGER.info(String.format("Reindex document %s.%s", module.getName(), documentName));
								DocumentQuery query = persistence.newDocumentQuery(document);
								try (AutoClosingIterable<PersistentBean> i = query.beanIterable()) {
									for (PersistentBean bean : i) {
										persistence.reindex(bean);
										persistence.evictCached(bean);
									}
								}
							}
						}
						finally {
							persistence.commit(false);
						}
					}
				}
			}
		}
	}
	
	private static boolean hasContent(Table table) {
		for (String name : table.fields.keySet()) {
			AttributeType attributeType = table.fields.get(name);
			if (AttributeType.content.equals(attributeType)) {
				return true;
			}
		}
		return false;
	}
}
