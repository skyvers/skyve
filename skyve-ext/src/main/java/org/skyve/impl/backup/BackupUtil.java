package org.skyve.impl.backup;
 
import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.math.BigDecimal;
import java.sql.Date;
import java.sql.Time;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;
import java.util.TreeMap;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.customer.ExportedReference;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.RDBMSDynamicPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.persistence.hibernate.HibernateContentPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.DataStore;
import org.skyve.util.Util;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nullable;

final class BackupUtil {

    private static final Logger LOGGER = LoggerFactory.getLogger(BackupUtil.class);
    private static final Logger COMMAND_LOGGER = Category.COMMAND.logger();

	private BackupUtil() {
		// nothing to see here
	}

	static Calendar GMT = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

	static void initialise(String customerName,
							String contentDirectory,
							String contentFileStorage,
							String databaseDialect,
							String databaseJdbcDriver,
							String databaseConnectionUrl,
							String databaseUsername,
							String databasePassword) 
	throws Exception {
		AbstractPersistence.IMPLEMENTATION_CLASS = HibernateContentPersistence.class;
		AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = RDBMSDynamicPersistence.class;
		AbstractContentManager.IMPLEMENTATION_CLASS = AbstractContentManager.class;
		UtilImpl.CONTENT_DIRECTORY = contentDirectory;
		UtilImpl.CONTENT_FILE_STORAGE = Boolean.parseBoolean(contentFileStorage);
		UtilImpl.DATA_STORE = new DataStore(databaseJdbcDriver, 
												databaseConnectionUrl, 
												databaseUsername, 
												databasePassword, 
												databaseDialect);
		UtilImpl.DDL_SYNC = false;
		
		ProvidedRepositoryFactory.set(new LocalDesignRepository());
		SuperUser user = new SuperUser();
		user.setCustomerName(customerName);
		user.setName("backup");
		AbstractPersistence.get().setUser(user);
		
		try (ContentManager cm = EXT.newContentManager()) {
			@SuppressWarnings("resource")
			AbstractContentManager acm = (AbstractContentManager) cm;
			acm.startup();
			Thread.sleep(2000);
		}
	}
	
	static void finalise() throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
			@SuppressWarnings("resource")
			AbstractContentManager acm = (AbstractContentManager) cm;
			acm.shutdown();
			Thread.sleep(2000);
		}
		AbstractPersistence p = (AbstractPersistence) CORE.getPersistence();
		p.rollback();
		p.disposeAllPersistenceInstances();
	}
	
	static Collection<Table> getTables() throws Exception {
		// A case insensitive keyed map of tables
		Map<String, Table> result = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
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

	static Collection<Table> getTablesForAllCustomers() throws Exception {
		// A case insensitive keyed map of tables
		Map<String, Table> result = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
		ProvidedRepository repository = ProvidedRepositoryFactory.get();
		for (String customerName : repository.getAllCustomerNames()) {
			Customer customer = repository.getCustomer(customerName);
			if (customer == null) {
				throw new MetaDataException(customerName + " does not exist.");
			}
			
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
		}
		
		return result.values();
	}
	
	static void writeTables(Collection<Table> tables, File toWriteTo) throws Exception {
		try (OutputStreamWriter out = new OutputStreamWriter(new FileOutputStream(toWriteTo), UTF_8)) {
			try (BufferedWriter bw = new BufferedWriter(out)) {
				for (Table table : tables) {
					bw.write(table.toJSON());
					bw.newLine();
				}
			}
		}
	}
	
	static Collection<Table> readTables(File toReadFrom) throws Exception {
		Collection<Table> result = new ArrayList<>();
		try (InputStreamReader in = new InputStreamReader(new FileInputStream(toReadFrom), UTF_8)) {
			try (BufferedReader br = new BufferedReader(in)) {
				String table = br.readLine();
				while (table != null) {
					result.add(Table.fromJSON(table));
					table = br.readLine();
				}
			}
		}
		return result;
	}
	
	static void writeScript(List<String> commands, File toWriteTo) throws Exception {
		try (OutputStreamWriter out = new OutputStreamWriter(new FileOutputStream(toWriteTo), UTF_8)) {
			try (BufferedWriter bw = new BufferedWriter(out)) {
				for (String command : commands) {
					bw.write(command);
					bw.write(';');
					bw.newLine();
				}
			}
		}
	}

	static List<String> readScript(File toReadFrom) throws Exception {
		List<String> result = new ArrayList<>();
		try (InputStreamReader in = new InputStreamReader(new FileInputStream(toReadFrom), UTF_8)) {
			try (BufferedReader br = new BufferedReader(in)) {
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
					persistence.newSQL(command).noTimeout().execute();
				}
				catch (Exception e) {
					LOGGER.error("Could not execute SQL {}", command);
					throw e;
				}
			}
		}
		finally {
			// Use this form of commit method because ADM_Uniqueness may not exist here
			((AbstractHibernatePersistence) persistence).commit(false, false);
		}
	}

	private static void addOrUpdate(Map<String, Table> tables, Customer customer, Document document) {
		Persistent persistent = document.getPersistent();
		if ((! document.isDynamic()) && document.isPersistable()) { // static persistent document
			@SuppressWarnings("null") // test above
			String persistentIdentifier = persistent.getPersistentIdentifier();
			String agnosticIdentifier = persistent.getAgnosticIdentifier();
			Table table = tables.get(agnosticIdentifier);
			if (table == null) {
				table = new Table(agnosticIdentifier, persistentIdentifier);
				tables.put(agnosticIdentifier, table);
				if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("Table definition created for {}", agnosticIdentifier);
			}

			table.addFieldsFromDocument(customer, document);
			
			// Process any references the document has
			List<ExportedReference> references = ((CustomerImpl) customer).getExportedReferences(document);
			if (references != null) {
				for (ExportedReference reference : references) {
					Persistent referencePersistent = reference.getPersistent();
					if (referencePersistent != null) {
						Document referencedDocument = customer.getModule(reference.getModuleName()).getDocument(customer, reference.getDocumentName());
						// Add joining table for collections pointing to static documents
						if (reference.isCollection() && (! referencedDocument.isDynamic())) {
							// child collections have no joining table
							if (! CollectionType.child.equals(reference.getType())) {
								String referenceFieldName = reference.getReferenceFieldName();
								org.skyve.metadata.model.document.Collection collection = (org.skyve.metadata.model.document.Collection) referencedDocument.getReferenceByName(referenceFieldName);
								if (collection.isPersistent()) {
									String ownerAgnosticIdentifier = referencePersistent.getAgnosticIdentifier();
									String ownerPersistentIdentifier = referencePersistent.getPersistentIdentifier();
									ExtensionStrategy strategy = referencePersistent.getStrategy();
									
									// If it is a collection defined on a polymorphically mapped document pointing to this document, find
									// all persistent derivations with a table name to use
									if (referencePersistent.isPolymorphicallyMapped()) {
										List<String> derivedModocs = ((CustomerImpl) customer).getDerivedDocuments(referencedDocument);
										for (String derivedModoc : derivedModocs) {
											int dotIndex = derivedModoc.indexOf('.');
											Module derivedModule = customer.getModule(derivedModoc.substring(0, dotIndex));
											Document derivedDocument = derivedModule.getDocument(customer, derivedModoc.substring(dotIndex + 1));
	
											if (derivedDocument.isPersistable()) {
												Persistent derivedPersistent = derivedDocument.getPersistent();
												@SuppressWarnings("null") // tested above in isPersistable()
												String ai = derivedPersistent.getAgnosticIdentifier();
												ownerAgnosticIdentifier = ai;
												ownerPersistentIdentifier = derivedPersistent.getPersistentIdentifier();
												String joinAgnosticIdentifier = ownerAgnosticIdentifier + '_' + referenceFieldName;
												String joinPersistentIdentifier = ownerPersistentIdentifier + '_' + referenceFieldName;
												if (! tables.containsKey(joinAgnosticIdentifier)) {
													JoinTable joinTable = new JoinTable(joinAgnosticIdentifier, joinPersistentIdentifier, ownerAgnosticIdentifier, ownerPersistentIdentifier, Boolean.TRUE.equals(collection.getOrdered()));
													tables.put(joinAgnosticIdentifier, joinTable);
													if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("Table definition created for {}", joinAgnosticIdentifier);
												}
											}
										}
									}
									// If it is a collection defined on a joined document pointing to this document, find 
									// the base document with the biz fields in its table name to use
									else if (ExtensionStrategy.joined.equals(strategy)) {
										// Find the ultimate document (the document with the biz fields)
										Document ultimateDocument = referencedDocument;
										Extends currentInherits = ultimateDocument.getExtends();
										while (currentInherits != null) {
											Module module = customer.getModule(ultimateDocument.getOwningModuleName());
											Document baseDocument = module.getDocument(customer, currentInherits.getDocumentName());
											currentInherits = null;
		
											if (baseDocument.isPersistable()) {
												Persistent basePersistent = baseDocument.getPersistent();
												@SuppressWarnings("null") // tested above
												ExtensionStrategy baseStrategy = basePersistent.getStrategy();
												// keep looking if joined
												if (ExtensionStrategy.joined.equals(baseStrategy)) {
													ultimateDocument = baseDocument;
													currentInherits = ultimateDocument.getExtends();
												}
												// stop at the base document if the strategy is null, single or monomorphic mapped
												else if (! basePersistent.isPolymorphicallyMapped()) {
													ultimateDocument = baseDocument;
												}
												// ignore a base document that is polymorphically mapped
											}
										}

										Persistent ultimatePersistent = ultimateDocument.getPersistent();
										@SuppressWarnings("null") // tested above at baseDocument.isPersistable()
										String ai = ultimatePersistent.getAgnosticIdentifier();
										ownerAgnosticIdentifier = ai;
										ownerPersistentIdentifier = ultimatePersistent.getPersistentIdentifier();
										Persistent referencedPersistent = referencedDocument.getPersistent();
										@SuppressWarnings("null") // tested above at baseDocument.isPersistable()
										String joinAgnosticIdentifier = referencedPersistent.getAgnosticIdentifier() + '_' + referenceFieldName;
										String joinPersistentIdentifier = referencedPersistent.getPersistentIdentifier() + '_' + referenceFieldName;
										if (! tables.containsKey(joinAgnosticIdentifier)) {
											JoinTable joinTable = new JoinTable(joinAgnosticIdentifier, joinPersistentIdentifier, ownerAgnosticIdentifier, ownerPersistentIdentifier, Boolean.TRUE.equals(collection.getOrdered()));
											tables.put(joinAgnosticIdentifier, joinTable);
											if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("Table definition created for {}", joinAgnosticIdentifier);
										}
									}
									else {
										String joinAgnosticIdentifier = ownerAgnosticIdentifier + '_' + referenceFieldName;
										String joinPersistentIdentifier = ownerPersistentIdentifier + '_' + referenceFieldName;
										if (! tables.containsKey(joinAgnosticIdentifier)) {
											JoinTable joinTable = new JoinTable(joinAgnosticIdentifier, joinPersistentIdentifier, ownerAgnosticIdentifier, ownerPersistentIdentifier, Boolean.TRUE.equals(collection.getOrdered()));
											tables.put(joinAgnosticIdentifier, joinTable);
											if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("Table definition created for {}", joinAgnosticIdentifier);
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

	static boolean hasBizCustomer(Table table) {
		boolean result = false;
		for (String fieldName : table.fields.keySet()) {
			if (Bean.CUSTOMER_NAME.equalsIgnoreCase(fieldName)) {
				result = true;
				break;
			}
		}
		return result;
	}
	
	static void secureSQL(StringBuilder sql, Table table, String customerName) {
		if (table instanceof JoinTable joinTable) {
			sql.append(" where ").append(PersistentBean.OWNER_COLUMN_NAME);
			sql.append(" in (select ").append(Bean.DOCUMENT_ID).append(" from ").append(joinTable.ownerPersistentIdentifier);
			if (UtilImpl.CUSTOMER == null) { // multi-tenant
				sql.append(" where ").append(Bean.CUSTOMER_NAME).append(" = '").append(customerName).append('\'');
			}
			sql.append(')');
		}
		else {
			if ((UtilImpl.CUSTOMER == null) && hasBizCustomer(table)) {
				sql.append(" where ").append(Bean.CUSTOMER_NAME).append(" = '").append(customerName).append('\'');
			}
		}
	}
	
	/**
	 * Redacts data depending on type for most scalar types.<br/>
	 * Returns value unchanged if redaction for this type is not (yet) supported.
	 * 
	 * @param attributeType - the type of attribute.
	 * @param value - the data to be obfuscated.
	 * @return the redacted {@link Object}.
	 * @author simeonsolomou
	 */
	static Object redactData(AttributeType attributeType, Object value) {
		return redactData(attributeType, value, null);
	}

	/**
	 * Redacts data depending on type for most scalar types.<br/>
	 * Returns value unchanged if redaction for this type is not (yet) supported.
	 * 
	 * @param attributeType - the type of attribute.
	 * @param value - the data to be obfuscated.
	 * @param maxLength - the maximum number of characters to redact to.
	 * @return the redacted {@link Object}.
	 * @author simeonsolomou
	 */
	static Object redactData(AttributeType attributeType, Object value, @Nullable Integer maxLength) {
		if (value != null) {
			switch (attributeType) {
			case association:
				// Nothing to see here
				break;
			case bool:
				// Nothing to see here
				break;
			case collection:
				// Nothing to see here
				break;
			case colour:
				// Nothing to see here
				break;
			case content:
				return null; // Nullify content fields
			case date:
				return redactDate((Date) value);
			case dateTime:
				return redactTimestamp((java.sql.Timestamp) value);
			case decimal10:
				return redactNumeric((BigDecimal) value);
			case decimal2:
				return redactNumeric((BigDecimal) value);
			case decimal5:
				return redactNumeric((BigDecimal) value);
			case enumeration:
				// Nothing to see here
				break;
			case geometry:
				return redactGeometry((Geometry) value);
			case id:
				return redactString((String) value, maxLength);
			case image:
				return null; // Nullify content fields
			case integer:
				return redactNumeric((Integer) value);
			case inverseMany:
				// Nothing to see here
				break;
			case inverseOne:
				// Nothing to see here
				break;
			case longInteger:
				return redactNumeric((Long) value);
			case markup:
				return redactString((String) value, maxLength);
			case memo:
				return redactString((String) value, maxLength);
			case text:
				return redactString((String) value, maxLength);
			case time:
				return redactTime((Time) value);
			case timestamp:
				return redactTimestamp((java.sql.Timestamp) value);
			default:
				break;
			}
		}
		return value;
	}
	
	/**
	 * Redacts a string by masking its middle part with asterisks.
	 *
	 * @param data - the string to be redacted.
	 * @param maxLength - the maximum number of characters to redact to.
	 * @return the redacted {@link String}.
	 * @author Ben Petito
	 */
	private static String redactString(String data, @Nullable Integer maxLength) {
		if (data == null) {
			return null;
		}

		// check if the data is an email address
		if (data.contains("@")) {
			int atIndex = data.indexOf("@");
			String beforeAt = redactSegment(data.substring(0, atIndex), maxLength);
			String afterAt = redactSegment(data.substring(atIndex + 1), maxLength);
			return beforeAt + "@" + afterAt;
		}

		return redactSegment(data, maxLength);
	}
	
	/**
	 * Redacts a segment of a string, only displaying a fixed number of chars at beginning & end.
	 * 
	 * @param data - the segment to be redacted.
	 * @param maxLength - the maximum number of characters to redact to.
	 * @return the redacted segment.
	 * @author Ben Petito
	 */
	private static String redactSegment(String data, @Nullable Integer maxLength) {
		// define the number of characters to keep at the beginning and end based on data length.
		int charsToShowAtStart = 2;
		int charsToShowAtEnd = 2;

		if (data.length() == 0) {
			return "";
		} else if (data.length() <= 1) {
			charsToShowAtStart = 0;
			charsToShowAtEnd = 0;
		} else if (data.length() <= 2) {
			charsToShowAtStart = 1;
			charsToShowAtEnd = 0;
		} else if (data.length() <= 4) {
			charsToShowAtStart = 1;
			charsToShowAtEnd = 1;
		}

		String start = data.substring(0, charsToShowAtStart);
		String end = data.substring(data.length() - charsToShowAtEnd);

		// calculate the number of asterisks to be used
		int asteriskCount = data.length() - charsToShowAtStart - charsToShowAtEnd;

		// if this is a short string...
		if (asteriskCount < 5) {
			// pad asterisk count (considering maximum length)
			if (maxLength == null || maxLength.intValue() >= (charsToShowAtStart + charsToShowAtEnd + 4)) {
				asteriskCount = 4;
			} else {
				asteriskCount = maxLength.intValue() - (charsToShowAtStart + charsToShowAtEnd);
			}
		}

		// set the max asterisk count to 10
		if (asteriskCount > 10) {
			asteriskCount = 10;
		}

		StringBuilder maskedSection = new StringBuilder();
		for (int i = 0; i < asteriskCount; i++) {
			maskedSection.append('*');
		}

		// remove any whitespace and re-assemble the redacted string
		return start.trim() + maskedSection + end.trim();
	}
	
	/**
	 * Redacts skyve numeric attributes by rounding to the nearest 10.
	 *
	 * @param data - the numeric to be redacted.
	 * @return The redacted numeric.
	 * @author simeonsolomou
	 */
	private static <T extends Number> T redactNumeric(T data) {
		if (data == null) {
			return null;
		}
		
		double doubleValue = data.doubleValue();
		double dividedByTen = Math.round(doubleValue / 10.0f);
		double div10 = dividedByTen * 10;
		
		if (data instanceof Integer) {
			int intValue = (int) Math.round(div10);
			@SuppressWarnings("unchecked")
			T result = (T) Integer.valueOf(intValue);
			return result;
		}
		else if (data instanceof Long) {
			long longValue = (long) div10;
			@SuppressWarnings("unchecked")
			T result = (T) Long.valueOf(longValue);
			return result;
		}
		else if (data instanceof BigDecimal) {
			@SuppressWarnings("unchecked")
			T result = (T) BigDecimal.valueOf(div10);
			return result;
		}
		
		return null;
	}
	
	/**
	 * Redacts {@link DateOnly} attribute by rounding it's value to the first day of the month.
	 * 
	 * @param data - the date to be redacted.
	 * @return the redacted date.
	 * @author simeonsolomou
	 */
	private static Date redactDate(Date data) {
		return Date.valueOf(data.toLocalDate().withDayOfMonth(1));
	}
	
	/**
	 * Redacts {@link TimeOnly} attribute by rounding it's value to the nearest hour.
	 * 
	 * @param data - the time to be redacted.
	 * @return the redacted time.
	 * @author simeonsolomou
	 */
	private static Time redactTime(Time data) {
		return Time.valueOf(data.toLocalTime().withMinute(0).withSecond(0).withNano(0));
	}
	
	/**
	 * Redacts {@link DateTime} attribute by rounding it's value to the first day of the month.
	 * 
	 * @param data - the date-time to be redacted
	 * @return The redacted date-time.
	 * @author simeonsolomou
	 */
	private static java.sql.Timestamp redactTimestamp(java.sql.Timestamp data) {
		return java.sql.Timestamp.valueOf(data.toLocalDateTime()
				.withDayOfMonth(1).withHour(0).withMinute(0));
	}
	
	/**
	 * Redacts {@link Geometry} attribute by rounding its latitude/longitude to the nearest whole number.
	 * 
	 * @param data - the geometry to be redacted.
	 * @return the redacted geometry.
	 * @author simeonsolomou
	 */
	private static Geometry redactGeometry(Geometry data) {
		Coordinate[] existingCoordinates = data.getCoordinates();
		
		Coordinate[] modifiedCoordinates = new Coordinate[existingCoordinates.length];
		for (int i = 0; i < existingCoordinates.length; i++) {
			double modifiedLongitude = Math.round(existingCoordinates[i].x);
			double modifiedLatitude = Math.round(existingCoordinates[i].y);
			modifiedCoordinates[i] = new Coordinate(modifiedLongitude, modifiedLatitude);
		}
		
		if (data instanceof Point) {
			return new GeometryFactory().createPoint(modifiedCoordinates[0]);
		}
		else if (data instanceof LineString) {
			return new GeometryFactory().createLineString(modifiedCoordinates);
		}
		else if (data instanceof Polygon) {
			return new GeometryFactory().createPolygon(modifiedCoordinates);
		}
		
		// Should never reach - other geometry types are deprecated
		return data;
	}

	/**
	 * Creates and returns the {@link File} for the extracted directory,
	 * only after validating that it is a valid Skyve backup.
	 * 
	 * @param extractDirName - the name of the directory for the backup to extract.
	 * @return the validated backup.
	 * @throws {@link IllegalArgumentException}
	 * @author simeonsolomou
	 */
	public static File validateSkyveBackup(String extractDirName) {
		String customerName = CORE.getUser().getCustomerName();
		String backupDirectoryPath = Util.getBackupDirectory() +
										"backup_" + customerName +
										File.separator + extractDirName;

		File backupDirectory = new File(backupDirectoryPath);
		if ((! backupDirectory.exists()) || (! backupDirectory.isDirectory())) {
			throw new DomainException(backupDirectoryPath + " is not a directory");
		}

		// Validate that there is at least one CSV file in root
		boolean hasCsvFile = backupDirectory.listFiles(new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				return name.toLowerCase().endsWith(".csv");
			}
		}).length > 0;
		if (! hasCsvFile) {
			throw new DomainException(
					"No valid Skyve CSV files were found in the expected location (the root of the ZIP)."
							+ " If you have modified this ZIP, please ensure that it has been correctly recompressed");
		}

		return backupDirectory;
	}
}