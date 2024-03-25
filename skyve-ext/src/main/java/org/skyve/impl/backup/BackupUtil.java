package org.skyve.impl.backup;
 
import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.math.BigDecimal;
import java.sql.Date;
import java.sql.Time;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeMap;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.customer.ExportedReference;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.RDBMSDynamicPersistence;
import org.skyve.impl.persistence.hibernate.HibernateContentPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.SensitivityType;
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

final class BackupUtil {

	private static final String DATA_SENSITIVITY_PROPERTY_NAME = "dataSensitivity";
	
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

	static Collection<Table> getTablesForAllCustomers() throws Exception {
		Map<String, Table> result = new TreeMap<>();
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
					Util.LOGGER.severe("Could not execute SQL " + command);
					throw e;
				}
			}
		}
		finally {
			persistence.commit(false);
		}
	}

	static void addOrUpdate(Map<String, Table> tables, Customer customer, Document document) {
		Persistent persistent = document.getPersistent();
		if ((! document.isDynamic()) && document.isPersistable()) { // static persistent document
			@SuppressWarnings("null") // test above
			String persistentIdentifier = persistent.getPersistentIdentifier();
			Table table = tables.get(persistentIdentifier);
			if (table == null) {
				table = new Table(persistentIdentifier);
				tables.put(persistentIdentifier, table);
			}

			table.addFieldsFromDocument(customer, document);
			
			// Process any references the document has
			List<ExportedReference> references = ((CustomerImpl) customer).getExportedReferences(document);
			if (references != null) {
				for (ExportedReference reference : references) {
					Persistent referencePersistent = reference.getPersistent();
					if (referencePersistent != null) {
						Document referencedDocument = customer.getModule(reference.getModuleName()).getDocument(customer,
																													reference.getDocumentName());
						// Add joining table for collections pointing to static documents
						if (reference.isCollection() && (! referencedDocument.isDynamic())) {
							// child collections have no joining table
							if (! CollectionType.child.equals(reference.getType())) {
								String referenceFieldName = reference.getReferenceFieldName();
								org.skyve.metadata.model.document.Collection collection = (org.skyve.metadata.model.document.Collection) referencedDocument.getReferenceByName(referenceFieldName);
								if (collection.isPersistent()) {
									String ownerTableName = referencePersistent.getPersistentIdentifier();
									ExtensionStrategy strategy = referencePersistent.getStrategy();
									
									// If it is a collection defined on a mapped document pointing to this document, find
									// all persistent derivations with a table name to use
									if (ExtensionStrategy.mapped.equals(strategy)) {
										List<String> derivedModocs = ((CustomerImpl) customer).getDerivedDocuments(referencedDocument);
										for (String derivedModoc : derivedModocs) {
											int dotIndex = derivedModoc.indexOf('.');
											Module derivedModule = customer.getModule(derivedModoc.substring(0, dotIndex));
											Document derivedDocument = derivedModule.getDocument(customer, derivedModoc.substring(dotIndex + 1));
	
											if (derivedDocument.isPersistable()) {
												@SuppressWarnings("null") // tested above
												String persistentName = derivedDocument.getPersistent().getName();
												ownerTableName = persistentName;
												String tableName = ownerTableName + '_' + referenceFieldName;
												if (! tables.containsKey(tableName)) {
													JoinTable joinTable = new JoinTable(tableName, ownerTableName, Boolean.TRUE.equals(collection.getOrdered()));
													tables.put(tableName, joinTable);
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
												@SuppressWarnings("null") // tested above
												ExtensionStrategy baseStrategy = baseDocument.getPersistent().getStrategy();
												// keep looking if joined
												if (ExtensionStrategy.joined.equals(baseStrategy)) {
													ultimateDocument = baseDocument;
													currentInherits = ultimateDocument.getExtends();
												}
												// stop at the base document if the strategy is null or single
												else if (! ExtensionStrategy.mapped.equals(baseStrategy)) {
													ultimateDocument = baseDocument;
												}
												// ignore a base document that is mapped
											}
										}

										@SuppressWarnings("null") // tested above at baseDocument.isPersistable()
										String ultimatePersistentName = ultimateDocument.getPersistent().getName();
										ownerTableName = ultimatePersistentName;
										@SuppressWarnings("null") // tested above at baseDocument.isPersistable()
										String tableName = referencedDocument.getPersistent().getName() + '_' + referenceFieldName;
										if (! tables.containsKey(tableName)) {
											JoinTable joinTable = new JoinTable(tableName, ownerTableName, Boolean.TRUE.equals(collection.getOrdered()));
											tables.put(tableName, joinTable);
										}
									}
									else {
										String tableName = ownerTableName + '_' + referenceFieldName;
										if (! tables.containsKey(tableName)) {
											JoinTable joinTable = new JoinTable(tableName, ownerTableName, Boolean.TRUE.equals(collection.getOrdered()));
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
		if (table instanceof JoinTable) {
			JoinTable joinTable = (JoinTable) table;
			sql.append(" where ").append(PersistentBean.OWNER_COLUMN_NAME);
			sql.append(" in (select ").append(Bean.DOCUMENT_ID).append(" from ").append(joinTable.ownerTableName);
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
	 * Fetch sensitivity index, calculated from ordinal value of {@link SensitivityType} selected in UI.
	 * 
	 * Returns 0 if no sensitivity level is selected.
	 * 
	 * @param bean DataMaintenance bean
	 */
	static int getSensitivityIndex(Bean bean) {
		if (bean != null) {
			Object sensitivityInput = BindUtil.get(bean, DATA_SENSITIVITY_PROPERTY_NAME);
			if (sensitivityInput != null) {
				return SensitivityType.valueOf(sensitivityInput.toString()).ordinal();
			}
		}
		
		return 0;
	}
	
	/**
	 * Constructs and returns a set of attributes (table.column) to redact.
	 * 
	 * @param sensitivityIndex Sensitivity benchmark. If any attribute has a greater than or equal to sensitivity score, it is redacted.
	 */
	static Set<String> getAttributesToRedact(int sensitivityIndex) {
		Customer customer = CORE.getPersistence().getUser().getCustomer();
		Set<String> attributesToRedact = new HashSet<>();
		
		if (sensitivityIndex == 0) {
			// Nothing to redact
			return attributesToRedact;
		}
		
		// Construct map of attributes to redact
		for (Module module : customer.getModules()) {
			for (Entry<String, DocumentRef> entry : module.getDocumentRefs().entrySet()) {
				DocumentRef documentRef = entry.getValue();
				if (documentRef.getOwningModuleName().equals(module.getName())) {
					Document document = module.getDocument(customer, entry.getKey());
					if (document.isPersistable()) {
						String tableName = document.getPersistent().getPersistentIdentifier();
						for (Attribute a : document.getAllAttributes(customer)) {
							int sensitivityScore = a.getSensitivity() != null ? a.getSensitivity().ordinal() : 0;
							if (sensitivityScore >= sensitivityIndex) {
								attributesToRedact.add(tableName + "." + a.getName());					}
						}
					}
				}
			}
		}
		
		return attributesToRedact;
	}
	
	/**
	 * Redacts data depending on type for most scalar types.
	 * 
	 * Returns value unchanged if redaction for this type is not (yet) supported.
	 *  
	 * @param value The Skyve record to be obfuscated
	 * 
	 * @author Simeon Solomou
	 */
	static Object redactData(AttributeType attributeType, Object value) {
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
				return redactString((String) value);
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
				return redactString((String) value);
			case memo:
				return redactString((String) value);
			case text:
				return redactString((String) value);
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
	 * @param data The string to be redacted
	 * @return The redacted string
	 * 
	 * @author Ben Petito
	 */
	public static String redactString(String data) {
		if (data == null) {
			return null;
		}

		// check if the data is an email address
		if (data.contains("@")) {
			int atIndex = data.indexOf("@");
			String beforeAt = redactSegment(data.substring(0, atIndex));
			String afterAt = redactSegment(data.substring(atIndex + 1));
			return beforeAt + "@" + afterAt;
		}

		return redactSegment(data);
	}
	
	/**
	 * Redacts a segment of a string, only displaying a fixed number of chars at beginning & end.
	 * 
	 * @param data The segment to be redacted
	 * @return The redacted segment 
	 * 
	 * @author Ben Petito
	 */
	private static String redactSegment(String data) {
		// Define the number of characters to keep at the beginning and end based on data length.
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
	 * @param data The numeric to be redacted
	 * @return The redacted numeric
	 * 
	 * @author Simeon Solomou
	 */
	@SuppressWarnings("unchecked")
	public static <T extends Number> T redactNumeric(T data) {
		if (data == null) {
			return null;
		}
		
		double doubleValue = data.doubleValue();
		double dividedByTen = Math.round(doubleValue / 10.0f);
		double result = dividedByTen * 10;
		
		if (data instanceof Integer) {
			int intValue = (int) Math.round(result);
			return (T) Integer.valueOf(intValue);
		}
		else if (data instanceof Long) {
			long longValue = (long) result;
			return (T) Long.valueOf(longValue);
		}
		else if (data instanceof BigDecimal) {
			return (T) BigDecimal.valueOf(result);
		}
		
		return null;
	}
	
	/**
	 * Redacts {@link DateOnly} attribute by rounding it's value to the first day of the month.
	 * 
	 * @param data The date to be redacted
	 * @return The redacted date
	 * 
	 * @author Simeon Solomou
	 */
	public static Date redactDate(Date data) {
		
		return Date.valueOf(data.toLocalDate()
				.withDayOfMonth(1));
	}
	
	/**
	 * Redacts {@link TimeOnly} attribute by rounding it's value to the nearest hour.
	 * 
	 * @param data The time to be redacted
	 * @return The redacted time
	 * 
	 * @author Simeon Solomou
	 */
	public static Time redactTime(Time data) {
		
		return Time.valueOf(data.toLocalTime()
				.withMinute(0).withSecond(0).withNano(0));
	}
	
	/**
	 * Redacts {@link DateTime} attribute by rounding it's value to the first day of the month.
	 * 
	 * @param data The date-time to be redacted
	 * @return The redacted date
	 * 
	 * @author Simeon Solomou
	 */
	public static java.sql.Timestamp redactTimestamp(java.sql.Timestamp data) {
		
		return java.sql.Timestamp.valueOf(data.toLocalDateTime()
				.withDayOfMonth(1).withHour(0).withMinute(0));
	}
	
	/**
	 * Redacts {@link Geometry} attribute by rounding its latitude/longitude to the nearest whole number.
	 * 
	 * @param data The geometry to be redacted
	 * @return The redacted geometry
	 * 
	 * @author Simeon Solomou
	 */
	public static Geometry redactGeometry(Geometry data) {
		
		Coordinate[] existingCoordinates = data.getCoordinates();
		
		Coordinate[] modifiedCoordinates = new Coordinate[existingCoordinates.length];
		for (int i = 0; i < existingCoordinates.length; i++) {
			double modifiedLongitude = Math.round(existingCoordinates[i].x);
			double modifiedLatitude = Math.round(existingCoordinates[i].y);
			modifiedCoordinates[i] = new Coordinate(modifiedLongitude, modifiedLatitude);
		}
		
		if (data.getGeometryType().equals("Point")) {
			return new GeometryFactory().createPoint(modifiedCoordinates[0]);
		}
		else if (data.getGeometryType().equals("LineString")) {
			return new GeometryFactory().createLineString(modifiedCoordinates);
		}
		else if (data.getGeometryType().equals("Polygon")) {
			return new GeometryFactory().createPolygon(modifiedCoordinates);
		}
		
		// Should never reach - other geometry types are deprecated
		return data;
	}
}
