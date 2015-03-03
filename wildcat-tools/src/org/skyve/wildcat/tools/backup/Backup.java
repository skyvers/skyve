package org.skyve.wildcat.tools.backup;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

import javax.jcr.Session;

import org.apache.jackrabbit.core.data.DataStoreException;
import org.hibernate.usertype.UserType;
import org.hibernatespatial.SpatialDialect;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.wildcat.content.ContentUtil;
import org.skyve.wildcat.content.StreamContent;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.wildcat.util.UtilImpl;
import org.supercsv.io.CsvMapWriter;
import org.supercsv.prefs.CsvPreference;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKTWriter;

/**
 * Tables and the content repository files are backed up by this.
 * The fields are added to the tables taking into account that 
 * there may be multiple documents mapped onto the same table.
 * But we only want one copy of each table.
 * The customer data is separated out in the data base.
 * 
 * Each content file contains an associated named properties file 
 * that contains all the information needed to construct the path
 * of the content node - ie module name and document name are not known to the table.
 */
public class Backup {
	private static void backup(Collection<Table> tables, String customerName) throws Exception {
		final String BACKUP_DIR_PREFIX = "./backup_";

		File directory = new File(BACKUP_DIR_PREFIX + customerName + File.separator);
		directory.mkdirs();

		UserType geometryUserType = null; // this is only created when we come across a geometry
		
		AbstractHibernatePersistence persistence = (AbstractHibernatePersistence) AbstractPersistence.get();
		try {
			Session jcrSession = null;

			try {
				// Don't close this connection
				@SuppressWarnings("resource")
				Connection connection = persistence.getConnection();
				for (Table table : tables) {
					try (Statement statement = connection.createStatement()) {
						StringBuilder sql = new StringBuilder(128);
						sql.append("select * from ").append(table.name);
						BackupUtil.secureSQL(sql, table, customerName);
						statement.execute(sql.toString());
						try (ResultSet resultSet = statement.getResultSet()) {
							UtilImpl.LOGGER.info("Backup " + table.name);
							try (FileWriter fw = new FileWriter(BACKUP_DIR_PREFIX + customerName + File.separator + table.name + ".csv",
																	false)) {
								CsvMapWriter writer = new CsvMapWriter(fw, CsvPreference.STANDARD_PREFERENCE);
								try {
									Map<String, Object> values = new TreeMap<>();
									String[] headers = new String[table.fields.size()];
									headers = table.fields.keySet().toArray(headers);
	
									writer.writeHeader(headers);
	
									while (resultSet.next()) {
										values.clear();
	
										for (String name : table.fields.keySet()) {
											AttributeType attributeType = table.fields.get(name);
											Object value = null;
	
											if (AttributeType.association.equals(attributeType) ||
													AttributeType.colour.equals(attributeType) ||
													AttributeType.memo.equals(attributeType) ||
													AttributeType.markup.equals(attributeType) ||
													AttributeType.text.equals(attributeType) ||
													AttributeType.enumeration.equals(attributeType)) {
												value = resultSet.getString(name);
												if (resultSet.wasNull()) {
													value = "";
												}
											}
											else if (attributeType == AttributeType.geometry) {
												if (geometryUserType == null) {
													SpatialDialect dialect = (SpatialDialect) Class.forName(UtilImpl.DIALECT).newInstance();
													geometryUserType = dialect.getGeometryUserType();
												}
												Geometry geometry = (Geometry) geometryUserType.nullSafeGet(resultSet, new String[] {name}, null);
												if (geometry == null) {
													value = "";
												}
												else {
													value = new WKTWriter().write(geometry);
												}
											}
											else if (attributeType == AttributeType.bool) {
												boolean booleanValue = resultSet.getBoolean(name);
												if (resultSet.wasNull()) {
													value = "";
												}
												else {
													value = Boolean.valueOf(booleanValue);
												}
											}
											else if ((attributeType == AttributeType.date) ||
														(attributeType == AttributeType.dateTime) ||
														(attributeType == AttributeType.time) ||
														(attributeType == AttributeType.timestamp)) {
												Date date = resultSet.getDate(name);
												if (resultSet.wasNull()) {
													value = "";
												}
												else {
													value = new Long(date.getTime());
												}
											}
											else if ((attributeType == AttributeType.decimal2) ||
														(attributeType == AttributeType.decimal5) ||
														(attributeType == AttributeType.decimal10)) {
												BigDecimal bigDecimal = resultSet.getBigDecimal(name);
												if (resultSet.wasNull()) {
													value = "";
												}
												else {
													value = bigDecimal;
												}
											}
											else if (attributeType == AttributeType.integer) {
												int intValue = resultSet.getInt(name);
												if (resultSet.wasNull()) {
													value = "";
												}
												else {
													value = new Integer(intValue);
												}
											}
											else if (attributeType == AttributeType.longInteger) {
												long longValue = resultSet.getLong(name);
												if (resultSet.wasNull()) {
													value = "";
												}
												else {
													value = new Long(longValue);
												}
											}
											else if (attributeType == AttributeType.content) {
												String stringValue = resultSet.getString(name);
												if (resultSet.wasNull()) {
													value = "";
												}
												else {
													// TODO backup content versions
													value = stringValue;
													if (jcrSession == null) {
														jcrSession = ContentUtil.getFullSession(customerName);
													}
													StreamContent content = null;
													try {
														content = ContentUtil.get(jcrSession, stringValue);
														try (InputStream cis = content.getStream()) {
															File contentDirectory = new File(directory.getAbsolutePath() + File.separator +
																								content.getBizModule() + File.separator +
																								content.getBizDocument() + File.separator +
																								stringValue.substring(0, 2) + File.separator +
																								stringValue.substring(2, 4) + File.separator +
																								stringValue.substring(4, 6));
															if (! contentDirectory.exists()) {
																contentDirectory.mkdirs();
															}
															try (FileOutputStream cos = new FileOutputStream(contentDirectory.getAbsolutePath() +
																												File.separator + stringValue + '.' +
																												content.getMimeType().getStandardFileSuffix())) {
																try (BufferedOutputStream bos = new BufferedOutputStream(cos)) {
																	byte[] bytes = new byte[1024]; // 1K
																	int bytesRead = 0;
																	while ((bytesRead = cis.read(bytes)) > 0) {
																		bos.write(bytes, 0, bytesRead);
																	}
																	bos.flush();
																}
															}
														}
													}
													catch (DataStoreException e) {
														if (e.getCause() instanceof FileNotFoundException) {
															System.err.println("*** ALTHOUGH THE FOLLOWING STACK TRACE DID NOT STOP THE BACKUP THIS IS SERIOUS");
															e.printStackTrace();
														}
														else {
															throw e;
														}
													}
												}
											}
	
											values.put(name, value);
										}
	
										writer.write(values, headers);
									}
								}
								finally {
									writer.close();
								}
							}
						}
					}
				}
			}
			finally {
				if (jcrSession != null) {
					jcrSession.logout();
				}
			}
		}
		finally {
			persistence.commit(true);
		}
	}

	public static void main(String[] args) throws Exception {
		if (args.length != 7) {
			System.err.println("args are <customerName> <content directory> <DB dialect> <DB driver> <DB URL> <DB username> <DB password>");
			System.exit(1);
		}
		BackupUtil.initialize(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
		Collection<Table> tables = BackupUtil.getTables();
		backup(tables, args[0]);
	}
}
