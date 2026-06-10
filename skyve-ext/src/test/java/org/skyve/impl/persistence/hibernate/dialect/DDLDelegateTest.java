package org.skyve.impl.persistence.hibernate.dialect;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.sql.Types;
import java.util.List;

import org.hibernate.boot.Metadata;
import org.hibernate.boot.model.relational.QualifiedTableName;
import org.hibernate.boot.model.relational.SqlStringGenerationContext;
import org.hibernate.mapping.Column;
import org.hibernate.mapping.Table;
import org.hibernate.tool.schema.extract.spi.ColumnInformation;
import org.hibernate.tool.schema.extract.spi.TableInformation;
import org.junit.Test;

@SuppressWarnings("static-method")
public class DDLDelegateTest {
	@Test
	public void isCharRecognisesAllJdbcCharacterTypes() {
		assertTrue(DDLDelegate.isChar(Types.CHAR));
		assertTrue(DDLDelegate.isChar(Types.VARCHAR));
		assertTrue(DDLDelegate.isChar(Types.LONGVARCHAR));
		assertTrue(DDLDelegate.isChar(Types.CLOB));
		assertTrue(DDLDelegate.isChar(Types.NCHAR));
		assertTrue(DDLDelegate.isChar(Types.NVARCHAR));
		assertTrue(DDLDelegate.isChar(Types.LONGNVARCHAR));
		assertTrue(DDLDelegate.isChar(Types.NCLOB));
		assertFalse(DDLDelegate.isChar(Types.INTEGER));
	}

	@Test
	public void missingColumnDoesNotRequireAlterColumn() {
		Column column = new Column("name");

		assertFalse(DDLDelegate.isAlterTableColumnChangeRequired(column, null));
	}

	@Test
	public void changedCharacterLengthRequiresAlterColumn() {
		Column column = new Column("name");
		column.setLength(64);

		assertTrue(DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo(Types.VARCHAR, 32, 0)));
	}

	@Test
	public void sameCharacterLengthDoesNotRequireAlterColumn() {
		Column column = new Column("name");
		column.setLength(64);

		assertFalse(DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo(Types.VARCHAR, 64, 0)));
	}

	@Test
	public void changedNumericPrecisionOrScaleRequiresAlterColumn() {
		Column column = new Column("amount");
		column.setPrecision(12);
		column.setScale(2);

		assertTrue(DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo(Types.NUMERIC, 10, 2)));
		assertTrue(DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo(Types.NUMERIC, 12, 4)));
		assertTrue(DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo(Types.FLOAT, 10, 2)));
		assertTrue(DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo(Types.REAL, 10, 2)));
		assertTrue(DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo(Types.DOUBLE, 10, 2)));
	}

	@Test
	public void sameNumericPrecisionAndScaleDoesNotRequireAlterColumn() {
		Column column = new Column("amount");
		column.setPrecision(12);
		column.setScale(2);

		assertFalse(DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo(Types.NUMERIC, 12, 2)));
	}

	@Test
	public void longTextFalsePositiveDoesNotRequireAlterColumn() {
		Column column = new Column("description");

		assertFalse(DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo(Types.VARCHAR, Integer.MAX_VALUE, 0)));
		assertFalse(DDLDelegate.isAlterTableColumnChangeRequired(column, columnInfo(Types.VARCHAR, AbstractH2SpatialDialect.TEXT_MAX_LENGTH, 0)));
	}

	@Test
	public void sqlAlterTableDDLBuildsAlterStatementForChangedColumn() throws Exception {
		Table table = new Table("Customer");
		Column column = new Column("name");
		column.setLength(64);
		column.setSqlType("varchar(64)");
		column.setDefaultValue("'unknown'");
		column.setNullable(false);
		column.setComment("customer name");
		table.addColumn(column);
		SQLServer2008SpatialDialect dialect = new SQLServer2008SpatialDialect();
		TableInformation tableInfo = mock(TableInformation.class);
		SqlStringGenerationContext context = mock(SqlStringGenerationContext.class);
		QualifiedTableName tableName = mock(QualifiedTableName.class);
		ColumnInformation nameColumnInfo = columnInfo(Types.VARCHAR, 32, 0);
		when(tableInfo.getName()).thenReturn(tableName);
		when(tableInfo.getColumn(any())).thenReturn(nameColumnInfo);
		when(context.format(any(QualifiedTableName.class))).thenReturn("dbo.Customer");

		List<String> sql = sqlAlterTableDDL(dialect, table, tableInfo, context, mock(Metadata.class));

		assertEquals(1, sql.size());
		assertEquals("alter table dbo.Customer alter column name varchar(64) default 'unknown' not null", sql.get(0));
	}

	@Test
	public void sqlAlterTableDDLBuildsPostgreSQLTypeAlterForNullableColumn() throws Exception {
		Table table = new Table("Customer");
		Column column = new Column("name");
		column.setLength(128);
		column.setSqlType("varchar(128)");
		column.setNullable(true);
		table.addColumn(column);
		PostgreSQL10SpatialDialect dialect = new PostgreSQL10SpatialDialect();
		TableInformation tableInfo = mock(TableInformation.class);
		SqlStringGenerationContext context = mock(SqlStringGenerationContext.class);
		QualifiedTableName tableName = mock(QualifiedTableName.class);
		ColumnInformation nameColumnInfo = columnInfo(Types.VARCHAR, 64, 0);
		when(tableInfo.getName()).thenReturn(tableName);
		when(tableInfo.getColumn(any())).thenReturn(nameColumnInfo);
		when(context.format(any(QualifiedTableName.class))).thenReturn("public.customer");

		List<String> sql = sqlAlterTableDDL(dialect, table, tableInfo, context, mock(Metadata.class));

		assertEquals(1, sql.size());
		assertThat(sql.get(0), containsString("alter column name type varchar(128)"));
	}

	@SuppressWarnings("boxing")
	private static ColumnInformation columnInfo(int typeCode, int columnSize, int decimalDigits) {
		ColumnInformation result = mock(ColumnInformation.class);
		when(result.getTypeCode()).thenReturn(typeCode);
		when(result.getColumnSize()).thenReturn(columnSize);
		when(result.getDecimalDigits()).thenReturn(decimalDigits);
		return result;
	}

	@SuppressWarnings("unchecked")
	private static List<String> sqlAlterTableDDL(SkyveDialect skyveDialect,
													Table table,
													TableInformation tableInfo,
													SqlStringGenerationContext context,
													Metadata metadata)
	throws Exception {
		Method method = DDLDelegate.class.getDeclaredMethod("sqlAlterTableDDL",
															SkyveDialect.class,
															Table.class,
															TableInformation.class,
															SqlStringGenerationContext.class,
															Metadata.class);
		method.setAccessible(true);
		return (List<String>) method.invoke(null, skyveDialect, table, tableInfo, context, metadata);
	}
}
