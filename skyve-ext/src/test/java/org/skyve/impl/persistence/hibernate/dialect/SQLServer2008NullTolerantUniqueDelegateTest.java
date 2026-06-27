package org.skyve.impl.persistence.hibernate.dialect;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.hibernate.boot.Metadata;
import org.hibernate.boot.model.relational.Database;
import org.hibernate.boot.model.relational.QualifiedTableName;
import org.hibernate.boot.model.relational.SqlStringGenerationContext;
import org.hibernate.dialect.SQLServer2008Dialect;
import org.hibernate.engine.jdbc.env.spi.JdbcEnvironment;
import org.hibernate.engine.jdbc.env.spi.QualifiedObjectNameFormatter;
import org.hibernate.mapping.Column;
import org.hibernate.mapping.Table;
import org.hibernate.mapping.UniqueKey;
import org.junit.Test;

@SuppressWarnings({"static-method", "deprecation"})
public class SQLServer2008NullTolerantUniqueDelegateTest {
	@Test
	public void addUniqueKeyCommandCreatesFilteredIndexForEveryColumn() {
		SQLServer2008NullTolerantUniqueDelegate delegate = new SQLServer2008NullTolerantUniqueDelegate(new SQLServer2008Dialect());
		UniqueKey uniqueKey = uniqueKey();
		Metadata metadata = metadataFormattingTableAs("dbo.Customer");

		String sql = delegate.getAlterTableToAddUniqueKeyCommand(uniqueKey, metadata);

		assertEquals("CREATE UNIQUE NONCLUSTERED INDEX UK_customer_email ON dbo.Customer (email, tenant_id) WHERE email IS NOT NULL AND tenant_id IS NOT NULL;",
						sql);
	}

	@Test
	public void contextAddUniqueKeyCommandDelegatesToFilteredIndexCommand() {
		SQLServer2008NullTolerantUniqueDelegate delegate = new SQLServer2008NullTolerantUniqueDelegate(new SQLServer2008Dialect());
		UniqueKey uniqueKey = uniqueKey();
		Metadata metadata = metadataFormattingTableAs("dbo.Customer");

		String sql = delegate.getAlterTableToAddUniqueKeyCommand(uniqueKey, metadata, mock(SqlStringGenerationContext.class));

		assertEquals("CREATE UNIQUE NONCLUSTERED INDEX UK_customer_email ON dbo.Customer (email, tenant_id) WHERE email IS NOT NULL AND tenant_id IS NOT NULL;",
						sql);
	}

	@Test
	public void dropUniqueKeyCommandDropsFilteredIndexFromTable() {
		SQLServer2008NullTolerantUniqueDelegate delegate = new SQLServer2008NullTolerantUniqueDelegate(new SQLServer2008Dialect());
		UniqueKey uniqueKey = uniqueKey();
		Metadata metadata = metadataFormattingTableAs("dbo.Customer");

		String sql = delegate.getAlterTableToDropUniqueKeyCommand(uniqueKey, metadata);

		assertEquals("DROP INDEX UK_customer_email ON dbo.Customer;", sql);
	}

	@Test
	public void contextDropUniqueKeyCommandDelegatesToDropIndexCommand() {
		SQLServer2008NullTolerantUniqueDelegate delegate = new SQLServer2008NullTolerantUniqueDelegate(new SQLServer2008Dialect());
		UniqueKey uniqueKey = uniqueKey();
		Metadata metadata = metadataFormattingTableAs("dbo.Customer");

		String sql = delegate.getAlterTableToDropUniqueKeyCommand(uniqueKey, metadata, mock(SqlStringGenerationContext.class));

		assertEquals("DROP INDEX UK_customer_email ON dbo.Customer;", sql);
	}

	private static UniqueKey uniqueKey() {
		Table table = new Table("Customer");
		UniqueKey result = new UniqueKey();
		result.setTable(table);
		result.setName("UK_customer_email");
		result.addColumn(new Column("email"));
		result.addColumn(new Column("tenant_id"));
		return result;
	}

	private static Metadata metadataFormattingTableAs(String formattedTableName) {
		Metadata metadata = mock(Metadata.class);
		Database database = mock(Database.class);
		JdbcEnvironment jdbcEnvironment = mock(JdbcEnvironment.class);
		QualifiedObjectNameFormatter formatter = mock(QualifiedObjectNameFormatter.class);
		when(metadata.getDatabase()).thenReturn(database);
		when(database.getJdbcEnvironment()).thenReturn(jdbcEnvironment);
		when(jdbcEnvironment.getQualifiedObjectNameFormatter()).thenReturn(formatter);
		when(formatter.format(any(QualifiedTableName.class), same(new SQLServer2008Dialect()))).thenReturn(formattedTableName);
		when(formatter.format(any(QualifiedTableName.class), any())).thenReturn(formattedTableName);
		return metadata;
	}
}
