package org.skyve.wildcat.tools.backup;
 
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.wildcat.content.AbstractContentManager;
import org.skyve.wildcat.content.elasticsearch.ESClient;
import org.skyve.wildcat.metadata.customer.CustomerImpl;
import org.skyve.wildcat.metadata.customer.CustomerImpl.ExportedReference;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.metadata.repository.LocalDesignRepository;
import org.skyve.wildcat.metadata.user.SuperUser;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.persistence.hibernate.HibernateElasticSearchPersistence;
import org.skyve.wildcat.util.UtilImpl;

final class BackupUtil {
	private BackupUtil() {
		// nothing to see here
	}
	
	static void initialize(String customerName,
							String contentDirectory,
							String databaseDialect,
							String databaseJdbcDriver,
							String databaseConnectionUrl,
							String databaseUsername,
							String databasePassword) 
	throws MetaDataException {
		AbstractPersistence.IMPLEMENTATION_CLASS = HibernateElasticSearchPersistence.class;
		AbstractContentManager.IMPLEMENTATION_CLASS = ESClient.class;
		UtilImpl.CONTENT_DIRECTORY = contentDirectory;
		UtilImpl.DIALECT = databaseDialect;
		UtilImpl.STANDALONE_DATABASE_JDBC_DRIVER = databaseJdbcDriver;
		UtilImpl.STANDALONE_DATABASE_CONNECTION_URL = databaseConnectionUrl;
		UtilImpl.STANDALONE_DATABASE_USERNAME = databaseUsername;
		UtilImpl.STANDALONE_DATABASE_PASSWORD = databasePassword;
		
		AbstractRepository.set(new LocalDesignRepository());
		SuperUser user = new SuperUser();
		user.setCustomerName(customerName);
		user.setName("backup");
		AbstractPersistence.get().setUser(user);
	}
	
	static java.util.Collection<Table> getTables() throws Exception {
		Map<String, Table> result = new TreeMap<>();
		Customer customer = AbstractPersistence.get().getUser().getCustomer();

		// insert all defined documents into the tables list
		for (Module module : customer.getModules()) {
			for (Entry<String, DocumentRef> entry : module.getDocumentRefs().entrySet()) {
				DocumentRef documentRef = entry.getValue();
				if (documentRef.getOwningModuleName().equals(module.getName())) {
					Document document = module.getDocument(customer, entry.getKey());
					addOrUpdate(result, customer, document);
				}
			}
		}

		return result.values();
	}
	
	private static void addOrUpdate(Map<String, Table> tables, Customer customer, Document document)
	throws MetaDataException {
		Persistent persistent = document.getPersistent();
		if ((persistent != null) && (persistent.getName() != null)) { // persistent document
			String persistentIdentifier = persistent.getPersistentIdentifier();
			Table table = tables.get(persistentIdentifier);
			if (table == null) {
				table = new Table(persistentIdentifier);
				tables.put(persistentIdentifier, table);
			}

			table.addFieldsFromDocument(document);
			
			// Process any references the document has
			List<ExportedReference> references = ((CustomerImpl) customer).getExportedReferences(document);
			if (references != null) {
				for (ExportedReference reference : references) {
					Persistent referencePersistent = reference.getPersistent();
					Document referencedDocument = customer.getModule(reference.getModuleName()).getDocument(customer,
																												reference.getDocumentName());
					// Add joining table for collections
					if (reference.isCollection()) {
						// child collections have no joining table
						if (! CollectionType.child.equals(reference.getType())) {
							// add the joining table to the front of the list
							String referenceFieldName = reference.getReferenceFieldName();
							Collection collection = (Collection) referencedDocument.getReferenceByName(referenceFieldName);
							if (collection.isPersistent()) {
								String tableName = referencePersistent.getPersistentIdentifier() + '_' + referenceFieldName;
								JoinTable joinTable = new JoinTable(tableName, referencePersistent.getPersistentIdentifier());
								tables.put(tableName, joinTable);
							}
						}
					}
				}
			}
		}
	}

	static void secureSQL(StringBuilder sql, Table table, String customerName) {
		if (table instanceof JoinTable) {
			JoinTable joinTable = (JoinTable) table;
			sql.append(" where owner_id in (select bizId from ").append(joinTable.ownerTableName);
			sql.append(" where bizCustomer = '").append(customerName).append("') ");
		}
		else {
			sql.append(" where bizCustomer = '").append(customerName).append('\'');
		}
	}
}
