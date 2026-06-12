package org.skyve.impl.persistence.hibernate.dialect;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.h2.util.JdbcUtils;
import org.h2.util.Utils.ClassFactory;
import org.h2gis.functions.factory.H2GISFunctions;
import org.hibernate.boot.Metadata;
import org.hibernate.boot.model.naming.Identifier;
import org.hibernate.boot.model.relational.Database;
import org.hibernate.boot.model.relational.Namespace;
import org.hibernate.boot.model.relational.SqlStringGenerationContext;
import org.hibernate.boot.model.relational.internal.SqlStringGenerationContextImpl;
import org.hibernate.dialect.Dialect;
import org.hibernate.engine.jdbc.env.spi.JdbcEnvironment;
import org.hibernate.mapping.Column;
import org.hibernate.mapping.Table;
import org.hibernate.resource.transaction.spi.DdlTransactionIsolator;
import org.hibernate.service.ServiceRegistry;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;
import org.hibernate.tool.schema.extract.spi.DatabaseInformation;
import org.hibernate.tool.schema.extract.spi.TableInformation;
import org.hibernate.tool.schema.internal.Helper;
import org.hibernate.tool.schema.internal.exec.JdbcContext;
import org.hibernate.tool.schema.spi.SchemaManagementTool;
import org.skyve.EXT;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

/**
 * Utility class that applies Hibernate DDL migration scripts to the database,
 * delegating schema-diffing to the Hibernate {@code StandardServiceRegistry}.
 */
public class DDLDelegate {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(DDLDelegate.class);

	/**
	 * Prevents instantiation of this static utility class.
	 */
    private DDLDelegate() {
    	// nothing to see here
    }

	/**
	 * Generates Skyve-specific alter-column DDL and optionally applies it to the live database.
	 *
	 * <p>Side effects: when {@code execute} is {@code true}, this method opens a datastore connection,
	 * logs each generated statement, and executes best-effort schema updates against physical tables.
	 *
	 * @param standardRegistry the Hibernate service registry backing schema tooling
	 * @param metadata the Hibernate metadata to compare against the live schema
	 * @param skyveDialect the Skyve dialect supplying vendor-specific schema rules
	 * @param execute whether generated statements should be executed immediately
	 * @return the generated alter-column statements, in execution order
	 * @throws SQLException if the datastore connection or schema inspection cannot be established
	 */
    @SuppressWarnings("java:S3776") // Complexity OK
    public static List<String> migrate(ServiceRegistry standardRegistry, Metadata metadata, SkyveDialect skyveDialect, boolean execute)
	throws SQLException {
		List<String> result = new ArrayList<>(20);
		
		try (Connection connection = EXT.getDataStoreConnection()) {
			connection.setAutoCommit(true);

			final DdlTransactionIsolator ddlTransactionIsolator = new DdlTransactionIsolator() {
				/**
				 * Releases resources owned by this isolator.
				 */
				@Override
				public void release() {
					// nothing to do as the connection is outside
				}
				
				/**
				 * Prepares the isolator before schema tooling work begins.
				 */
				@Override
				public void prepare() {
					// nothing to do as the connection is outside
				}
				
				/**
				 * Returns the JDBC context associated with this isolator.
				 *
				 * @return null because this isolator is backed directly by an external connection
				 */
				@Override
				public JdbcContext getJdbcContext() {
					return null;
				}
				
				/**
				 * Returns the externally managed isolated connection used for DDL operations.
				 *
				 * @return the datastore connection opened by migrate
				 */
				@Override
				public Connection getIsolatedConnection() {
					return connection;
				}
			};

			if (RDBMS.h2.equals(skyveDialect.getRDBMS())) {
				JdbcUtils.addClassFactory(new ClassFactory() {
					/**
					 * Indicates whether this factory can attempt class resolution.
					 *
					 * @param name the class name requested by H2
					 * @return true to delegate all class loading requests to this factory
					 */
					@Override
					public boolean match(String name) {
						return true;
					}

					/**
					 * Loads a class through the current thread context class loader.
					 *
					 * @param name the fully qualified class name to load
					 * @return the resolved class
					 * @throws ClassNotFoundException if the class cannot be loaded
					 */
					@Override
					public Class<?> loadClass(String name) throws ClassNotFoundException {
						return Thread.currentThread().getContextClassLoader().loadClass(name);
					}
				});
				H2GISFunctions.load(connection);
			}
			
			SqlStringGenerationContext sqlStringGenerationContext = SqlStringGenerationContextImpl.fromConfigurationMap(
																			standardRegistry.getService(JdbcEnvironment.class),
																			metadata.getDatabase(),
																			Collections.emptyMap());
			final SchemaManagementTool tool = standardRegistry.getService(SchemaManagementTool.class);
			final DatabaseInformation databaseInformation = Helper.buildDatabaseInformation(standardRegistry,
																								ddlTransactionIsolator,
																								sqlStringGenerationContext,
																								tool);
			try (Statement statement = connection.createStatement()) {
				final Database database = metadata.getDatabase();
				for (Namespace namespace : database.getNamespaces()) {
					for (Table table : namespace.getTables()) {
						if (table.isPhysicalTable()) {
							final TableInformation tableInformation = databaseInformation.getTableInformation(table.getQualifiedTableName());
							if (tableInformation != null && tableInformation.isPhysicalTable()) {
								for (String ddl : sqlAlterTableDDL(skyveDialect,
																	table, 
																	tableInformation, 
																	sqlStringGenerationContext,
																	metadata)) {
	                        		result.add(ddl);
	                        		if (execute) {
		                        		LOGGER.info(ddl);
		                        		try {
		                        			statement.executeUpdate(ddl);
		                        		}
		                        		catch (Exception e) {
		                    				LOGGER.error("Could not apply skyve extra schema update of {}", ddl, e);
		                        		}
	                        		}
								}
							}
						}
					}
				}
			}
		}
		return result;
	}
	
	/**
	 * Builds alter-column DDL statements for an existing table by comparing mapped columns with live metadata.
	 *
	 * <p>This supplements Hibernate's default schema update behavior, which commonly adds missing columns
	 * but does not emit alter-column statements for type, length, precision, or scale changes.
	 *
	 * @param skyveDialect the Skyve-aware dialect that supplies alter-column syntax and comparison rules
	 * @param table the mapped Hibernate table definition
	 * @param tableInfo the extracted live table metadata
	 * @param sqlStringGenerationContext the SQL rendering context used to format table identifiers
	 * @param metadata the Hibernate metadata used for SQL type resolution
	 * @return iterable alter-column DDL statements for columns requiring changes
	 */
	// from org.hibernate.mapping.Table
	@SuppressWarnings("java:S3776") // Complexity OK
	private static Iterable<String> sqlAlterTableDDL(SkyveDialect skyveDialect, 
														Table table, 
														TableInformation tableInfo,
														SqlStringGenerationContext sqlStringGenerationContext,
														Metadata metadata) {
		List<String> result = new ArrayList<>();
		
		final Dialect dialect = (Dialect) skyveDialect;
		final String tableName = sqlStringGenerationContext.format(tableInfo.getName());

		StringBuilder root = new StringBuilder(dialect.getAlterTableString(tableName));
		root.append(' ').append(skyveDialect.getModifyColumnString());

		Iterator<?> iter = table.getColumnIterator();
		while (iter.hasNext()) {
			final Column column = (Column) iter.next();
			final ColumnInformation columnInfo = tableInfo.getColumn(Identifier.toIdentifier(column.getName(), column.isQuoted()));
			if (skyveDialect.isAlterTableColumnChangeRequired(column, columnInfo)) {
				StringBuilder alter = new StringBuilder(root.toString())
						.append(' ')
						.append(column.getQuotedName(dialect))
						.append(' ');
				if (RDBMS.postgresql.equals(skyveDialect.getRDBMS())) {
					alter.append("type ");
				}
				alter.append(column.getSqlType(dialect, metadata));

				String defaultValue = column.getDefaultValue();
				if (defaultValue != null) {
					alter.append(" default ").append(defaultValue);
				}

				if (column.isNullable()) {
					alter.append(dialect.getNullColumnString());
				}
				else {
					alter.append(" not null");
				}

				String columnComment = column.getComment();
				if (columnComment != null) {
					alter.append(dialect.getColumnComment(columnComment));
				}

				alter.append(dialect.getAddColumnSuffixString());

				result.add(alter.toString());
			}
		}

		return result;
	}
	
	/**
	 * Determines whether a mapped column differs from the live column metadata enough to require alter-column DDL.
	 *
	 * @param column the mapped Hibernate column definition
	 * @param columnInfo the extracted live database column metadata
	 * @return true when a type/size/precision/scale difference requires a column change statement, otherwise false
	 */
	static final boolean isAlterTableColumnChangeRequired(Column column, ColumnInformation columnInfo) {
/*
		if (columnInfo != null) {
			System.out.println("" + column.getSqlType() + " : " + 
								column.getSqlTypeCode() + " : " + 
								column.getLength() + " : " + 
								column.getPrecision() + " : " +
								column.getScale() + " : " + 
								column.getTypeIndex() + " = " +
								columnInfo.getColumnSize() + " : " +
								columnInfo.getDecimalDigits() + " : " + 
								columnInfo.getTypeCode() + " : " + 
								columnInfo.getTypeName() + " : " + 
								columnInfo.getColumnIdentifier());
		}
*/
		int typeCode = (columnInfo == null) ? 0 : columnInfo.getTypeCode();

		boolean result = (columnInfo != null) && // the column exists
							// char column and lengths are different
							(
								(isChar(typeCode) && (column.getLength() != columnInfo.getColumnSize()))
								||
								// decimal column and scales are different
								(((typeCode == Types.FLOAT) || 
										(typeCode == Types.REAL) || 
										(typeCode == Types.DOUBLE) ||
										(typeCode == Types.NUMERIC)) &&
									((column.getScale() != columnInfo.getDecimalDigits()) ||
										(column.getPrecision() != columnInfo.getColumnSize()))));
		// cater for longvarchar / text / clob / varchar(max) false positives
		if (result) {
			if ((column.getLength() == 255) && 
					(column.getPrecision() == 19) && 
					(column.getScale() == 2) &&
					(column.getTypeIndex() == 0) &&
					(typeCode == Types.VARCHAR) &&
					(columnInfo != null) &&
					((columnInfo.getColumnSize() == Integer.MAX_VALUE) ||
						// Cater for H2 2.x TEXT fields.
						(columnInfo.getColumnSize() == AbstractH2SpatialDialect.TEXT_MAX_LENGTH)) &&
					(columnInfo.getDecimalDigits() == 0)) {
				result = false;
			}
		}
		
		return result;
	}
	
	/**
	 * Tests whether a JDBC SQL type code represents a character-based textual type.
	 *
	 * @param typeCode the JDBC SQL type code to evaluate
	 * @return true when the type code is a char/varchar/clob variant, otherwise false
	 */
	static boolean isChar(int typeCode) {
		return (typeCode == Types.CHAR) ||
				(typeCode == Types.VARCHAR) || 
				(typeCode == Types.LONGVARCHAR) ||
				(typeCode == Types.CLOB) ||
				(typeCode == Types.NCHAR) ||
				(typeCode == Types.NVARCHAR) || 
				(typeCode == Types.LONGNVARCHAR) ||
				(typeCode == Types.NCLOB);
	}
}
