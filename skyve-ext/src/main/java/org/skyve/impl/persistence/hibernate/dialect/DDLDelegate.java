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
import org.skyve.impl.util.UtilImpl;

public class DDLDelegate {
	public static List<String> migrate(ServiceRegistry standardRegistry, Metadata metadata, SkyveDialect skyveDialect, boolean execute)
	throws SQLException {
		List<String> result = new ArrayList<>(20);
		
		try (Connection connection = EXT.getDataStoreConnection()) {
			connection.setAutoCommit(true);

			final DdlTransactionIsolator ddlTransactionIsolator = new DdlTransactionIsolator() {
				@Override
				public void release() {
					// nothing to do as the connection is outside
				}
				
				@Override
				public void prepare() {
					// nothing to do as the connection is outside
				}
				
				@Override
				public JdbcContext getJdbcContext() {
					return null;
				}
				
				@Override
				public Connection getIsolatedConnection() {
					return connection;
				}
			};

			if (RDBMS.h2.equals(skyveDialect.getRDBMS())) {
				JdbcUtils.addClassFactory(new ClassFactory() {
					@Override
					public boolean match(String name) {
						return true;
					}

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
		                        		UtilImpl.LOGGER.info(ddl);
		                        		try {
		                        			statement.executeUpdate(ddl);
		                        		}
		                        		catch (Exception e) {
		                    				UtilImpl.LOGGER.severe("Could not apply skyve extra schema update of " + ddl);
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
		return result;
	}
	
	/**
     * This method exists because by default, hibernate does not issue "alter table alter/modify column" statements only
     * "alter table add column" statements. So this hack allows alter table modify/alter column when the type, length or precision
     * has changed. The "modify column" syntax is outside the scope of the hibernate dialect classes and so its added to SkyveDialect.
     */
	// from org.hibernate.mapping.Table
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
