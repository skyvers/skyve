package org.skyve.impl.persistence.hibernate.dialect;

import java.util.Iterator;

import org.hibernate.boot.Metadata;
import org.hibernate.dialect.Dialect;
import org.hibernate.dialect.unique.DefaultUniqueDelegate;
import org.hibernate.engine.jdbc.env.spi.JdbcEnvironment;
import org.hibernate.engine.jdbc.env.spi.QualifiedObjectNameFormatter;
import org.hibernate.mapping.Column;
import org.hibernate.mapping.UniqueKey;

/**
 * Use SQLServer "CREATE UNIQUE NONCLUSTERED INDEX UniqueKey ON Table(Column) WHERE Column IS NOT NULL"
 * instead of defining unique Constraints as the toy database considers NULL
 * to be a value and raises a constraint violation.
 * 
 * @author mike
 */
public class SQLServer2008NullTolerantUniqueDelegate extends DefaultUniqueDelegate {
	public SQLServer2008NullTolerantUniqueDelegate(Dialect dialect) {
		super(dialect);
	}
	
	@Override
	public String getAlterTableToAddUniqueKeyCommand(UniqueKey uniqueKey, Metadata metadata) {
		final JdbcEnvironment jdbcEnvironment = metadata.getDatabase().getJdbcEnvironment();
		@SuppressWarnings("deprecation")
		final QualifiedObjectNameFormatter formatter = jdbcEnvironment.getQualifiedObjectNameFormatter();
		final String tableName = formatter.format(uniqueKey.getTable().getQualifiedTableName(), dialect);
		final String constraintName = dialect.quote(uniqueKey.getName());

		StringBuilder result = new StringBuilder(128);
		result.append("CREATE UNIQUE NONCLUSTERED INDEX ").append(constraintName).append(" ON ");
		result.append(tableName).append(" (");
		final Iterator<Column> i = uniqueKey.getColumnIterator();
		while (i.hasNext()) {
			final Column column = i.next();
			result.append(column.getQuotedName(dialect));
			if (i.hasNext()) {
				result.append(", ");
			}
		}
		result.append(')');

		boolean firstColumn = true;
		for (Column column : uniqueKey.getColumns()) {
			if (firstColumn) {
				result.append(" WHERE ");
				firstColumn = false;
			}
			else {
				result.append(" AND ");
			}
			result.append(column.getQuotedName(dialect)).append(" IS NOT NULL");
		}
		result.append(';');
		
		return result.toString();
	}
	
	@Override
	public String getAlterTableToDropUniqueKeyCommand(UniqueKey uniqueKey, Metadata metadata) {
		StringBuilder result = new StringBuilder(64);

		final JdbcEnvironment jdbcEnvironment = metadata.getDatabase().getJdbcEnvironment();
		@SuppressWarnings("deprecation")
		final QualifiedObjectNameFormatter formatter = jdbcEnvironment.getQualifiedObjectNameFormatter();
		final String tableName = formatter.format(uniqueKey.getTable().getQualifiedTableName(), dialect);
		result.append("DROP INDEX ").append(dialect.quote(uniqueKey.getName()));
		result.append(" ON ").append(tableName).append(';');
		
		return result.toString();
	}
}
