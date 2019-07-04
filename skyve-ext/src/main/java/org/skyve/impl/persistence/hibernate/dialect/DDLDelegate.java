package org.skyve.impl.persistence.hibernate.dialect;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.hibernate.boot.Metadata;
import org.hibernate.boot.model.naming.Identifier;
import org.hibernate.boot.model.relational.Database;
import org.hibernate.boot.model.relational.Namespace;
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
import org.skyve.EXT;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS;
import org.skyve.impl.util.UtilImpl;

import geodb.GeoDB;

public class DDLDelegate {
	public static List<String> migrate(ServiceRegistry standardRegistry, Metadata metadata, SkyveDialect skyveDialect, boolean execute)
	throws SQLException {
		List<String> result = new ArrayList<>(20);
		
		try (Connection connection = EXT.getDataStoreConnection()) {
			connection.setAutoCommit(true);

			// Ensure that a H2 database is spatially enabled
			if (execute && (skyveDialect instanceof H2SpatialDialect)) {
				GeoDB.InitGeoDB(connection);
			}

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
		
			final DatabaseInformation databaseInformation = Helper.buildDatabaseInformation(
																		standardRegistry,
																		ddlTransactionIsolator,
																		metadata.getDatabase().getDefaultNamespace().getName());
			try (Statement statement = connection.createStatement()) {
				final Database database = metadata.getDatabase();
				for (Namespace namespace : database.getNamespaces()) {
					for (Table table : namespace.getTables()) {
						if (table.isPhysicalTable()) {
							final TableInformation tableInformation = databaseInformation.getTableInformation(table.getQualifiedTableName());
							if (tableInformation != null && tableInformation.isPhysicalTable()) {
								for (String ddl : sqlAlterTableDDL(skyveDialect, table, tableInformation, metadata)) {
	                        		result.add(ddl);
	                        		if (execute) {
		                        		UtilImpl.LOGGER.info(ddl);
	                        			statement.executeUpdate(ddl);
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
														Metadata metadata) {
		List<String> result = new ArrayList<>();
		
		final Dialect dialect = (Dialect) skyveDialect;
		final JdbcEnvironment jdbcEnvironment = metadata.getDatabase().getJdbcEnvironment();

		final String tableName = jdbcEnvironment.getQualifiedObjectNameFormatter().format(
																	tableInfo.getName(), dialect);

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
*/
		int typeCode = (columnInfo == null) ? 0 : columnInfo.getTypeCode();

		boolean result = (columnInfo != null) && // the column exists
							// char column and lengths are different
							(
								(((typeCode == Types.VARCHAR) || 
										(typeCode == Types.CHAR) || 
										(typeCode == Types.LONGVARCHAR) ||
										(typeCode == Types.LONGNVARCHAR)) && 
										(column.getLength() != columnInfo.getColumnSize()))
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
					(columnInfo.getColumnSize() == Integer.MAX_VALUE) &&
					(columnInfo.getDecimalDigits() == 0)) {
				result = false;
			}
		}
		
		return result;
	}
}
