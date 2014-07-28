package org.skyve.wildcat.persistence.hibernate.dialect;

import java.sql.Types;

import org.hibernate.Hibernate;
import org.hibernate.dialect.function.NoArgSQLFunction;
import org.hibernate.engine.RowSelection;
import org.hibernatespatial.sqlserver.SQLServerSpatialDialect;
import org.skyve.wildcat.persistence.hibernate.dialect.pagination.SQLServer2005LimitHandler;

public class SQLServer2005SpatialDialect extends SQLServerSpatialDialect {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 5356790532797416899L;

	public SQLServer2005SpatialDialect() {
		super();

		// From Hibernate 4
		// HHH-3965 fix
		// As per http://www.sql-server-helper.com/faq/sql-server-2005-varchar-max-p01.aspx
		// use varchar(max) and varbinary(max) instead of TEXT and IMAGE types
		registerColumnType( Types.BLOB, "varbinary(MAX)" );
		registerColumnType( Types.VARBINARY, "varbinary(MAX)" );
		registerColumnType( Types.VARBINARY, 8000, "varbinary($l)" );
		registerColumnType( Types.LONGVARBINARY, "varbinary(MAX)" );

		registerColumnType( Types.CLOB, "varchar(MAX)" );
		registerColumnType( Types.LONGVARCHAR, "varchar(MAX)" );
		registerColumnType( Types.VARCHAR, "varchar(MAX)" );
		registerColumnType( Types.VARCHAR, 8000, "varchar($l)" );

		registerColumnType( Types.BIGINT, "bigint" );
		registerColumnType( Types.BIT, "bit" );
		
		registerFunction("row_number", new NoArgSQLFunction("row_number", Hibernate.INTEGER, true));
	}

	@Override
	public String getLimitString(String querySelect, int offset, int limit) {
		RowSelection rs = new RowSelection();
		rs.setFirstRow(Integer.valueOf(offset));
		rs.setMaxRows(Integer.valueOf(limit));
		String result = new SQLServer2005LimitHandler(querySelect, rs).getProcessedSql();
		return result;
	}

	@Override
	public boolean supportsLimit() {
		return true;
	}

	
	@Override
	public boolean supportsLimitOffset() {
		return true;
	}

	@Override
	public boolean supportsVariableLimit() {
		return false;
	}

	@Override
	public boolean useMaxForLimit() {
		return false;
	}
}
