package org.skyve.impl.backup;
 
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.elasticsearch.ESClient;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.customer.CustomerImpl.ExportedReference;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.HibernateElasticSearchPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.persistence.DataStore;
import org.skyve.util.Util;

final class BackupUtil {
	private BackupUtil() {
		// nothing to see here
	}
	
	static void initialise(String customerName,
							String contentDirectory,
							String contentFileStorage,
							String databaseDialect,
							String databaseJdbcDriver,
							String databaseConnectionUrl,
							String databaseUsername,
							String databasePassword) 
	throws Exception {
		AbstractPersistence.IMPLEMENTATION_CLASS = HibernateElasticSearchPersistence.class;
		AbstractContentManager.IMPLEMENTATION_CLASS = ESClient.class;
		UtilImpl.CONTENT_DIRECTORY = contentDirectory;
		UtilImpl.CONTENT_FILE_STORAGE = Boolean.parseBoolean(contentFileStorage);
		UtilImpl.DATA_STORE = new DataStore(databaseJdbcDriver, 
												databaseConnectionUrl, 
												databaseUsername, 
												databasePassword, 
												databaseDialect);
		UtilImpl.DDL_SYNC = false;
		
		AbstractRepository.set(new LocalDesignRepository());
		SuperUser user = new SuperUser();
		user.setCustomerName(customerName);
		user.setName("backup");
		AbstractPersistence.get().setUser(user);
		
		try (AbstractContentManager cm = (AbstractContentManager) EXT.newContentManager()) {
			cm.init();
			Thread.sleep(2000);
		}
	}
	
	static void finalise() throws Exception {
		try (AbstractContentManager cm = (AbstractContentManager) EXT.newContentManager()) {
			cm.dispose();
			Thread.sleep(2000);
		}
		AbstractPersistence p = (AbstractPersistence) CORE.getPersistence();
		p.rollback();
		p.disposeAllPersistenceInstances();
	}
	
	static Collection<Table> getTables() throws Exception {
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
	
	static void writeTables(Collection<Table> tables, File toWriteTo)
	throws Exception {
		try (FileWriter fw = new FileWriter(toWriteTo)) {
			try (BufferedWriter bw = new BufferedWriter(fw)) {
				for (Table table : tables) {
					bw.write(table.toJSON());
					bw.newLine();
				}
			}
		}
	}
	
	static Collection<Table> readTables(File toReadFrom) 
	throws Exception {
		Collection<Table> result = new ArrayList<>();
		try (FileReader fr = new FileReader(toReadFrom)) {
			try (BufferedReader br = new BufferedReader(fr)) {
				String table = br.readLine();
				while (table != null) {
					result.add(Table.fromJSON(table));
					table = br.readLine();
				}
			}
		}
		return result;
	}
	
	static void writeScript(List<String> commands, File toWriteTo) 
	throws Exception {
		try (FileWriter fw = new FileWriter(toWriteTo)) {
			try (BufferedWriter bw = new BufferedWriter(fw)) {
				for (String command : commands) {
					bw.write(command);
					bw.write(';');
					bw.newLine();
				}
			}
		}
	}

	static List<String> readScript(File toReadFrom) 
	throws Exception {
		List<String> result = new ArrayList<>();
		try (FileReader fr = new FileReader(toReadFrom)) {
			try (BufferedReader br = new BufferedReader(fr)) {
				String command = br.readLine();
				while (command != null) {
					if (command.endsWith(";")) {
						command = command.substring(0, command.length() - 1);
					}
					result.add(command);
					command = br.readLine();
				}
			}
		}
		
		return result;
	}
	
	static void executeScript(List<String> script) 
	throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		try {
			persistence.begin();

			for (String command : script) { 
				try {
					persistence.newSQL(command).execute();
				}
				catch (Exception e) {
					Util.LOGGER.severe("Could not execute SQL " + command);
					throw e;
				}
			}
		}
		finally {
			persistence.commit(false);
		}
	}

	static void addOrUpdate(Map<String, Table> tables, Customer customer, Document document)
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
					if (referencePersistent != null) {
						Document referencedDocument = customer.getModule(reference.getModuleName()).getDocument(customer,
																													reference.getDocumentName());
						// Add joining table for collections
						if (reference.isCollection()) {
							// child collections have no joining table
							if (! CollectionType.child.equals(reference.getType())) {
								String referenceFieldName = reference.getReferenceFieldName();
								org.skyve.metadata.model.document.Collection collection = (org.skyve.metadata.model.document.Collection) referencedDocument.getReferenceByName(referenceFieldName);
								if (collection.isPersistent()) {
									String ownerTableName = referencePersistent.getPersistentIdentifier();

									// If it is a collection defined on a mapped document pointing to this document, find
									// all persistent derivations with a table name to use
									if (ExtensionStrategy.mapped.equals(referencePersistent.getStrategy())) {
										List<String> derivedModocs = ((CustomerImpl) customer).getDerivedDocuments(referencedDocument);
										for (String derivedModoc : derivedModocs) {
											int dotIndex = derivedModoc.indexOf('.');
											Module derivedModule = customer.getModule(derivedModoc.substring(0, dotIndex));
											Document derivedDocument = derivedModule.getDocument(customer, derivedModoc.substring(dotIndex + 1));
	
											Persistent derivedPersistent = derivedDocument.getPersistent();
											if ((derivedPersistent != null) && (derivedPersistent.getName() != null)) {
												ownerTableName = derivedPersistent.getName();
												String tableName = ownerTableName + '_' + referenceFieldName;
												if (! tables.containsKey(tableName)) {
													JoinTable joinTable = new JoinTable(tableName, ownerTableName);
													tables.put(tableName, joinTable);
												}
											}
										}
									}
									else {
										String tableName = ownerTableName + '_' + referenceFieldName;
										if (! tables.containsKey(tableName)) {
											JoinTable joinTable = new JoinTable(tableName, ownerTableName);
											tables.put(tableName, joinTable);
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

	static void secureSQL(StringBuilder sql, Table table, String customerName) {
		if (table instanceof JoinTable) {
			JoinTable joinTable = (JoinTable) table;
			sql.append(" where owner_id in (select bizId from ").append(joinTable.ownerTableName);
			sql.append(" where bizCustomer = '").append(customerName).append("') ");
		}
		else {
			if (! table.joinedExtensionOnly) {
				sql.append(" where bizCustomer = '").append(customerName).append('\'');
			}
		}
	}
}
