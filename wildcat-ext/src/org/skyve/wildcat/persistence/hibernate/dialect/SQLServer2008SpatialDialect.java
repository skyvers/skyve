package org.skyve.wildcat.persistence.hibernate.dialect;

import java.sql.Types;

public class SQLServer2008SpatialDialect extends SQLServer2005SpatialDialect {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -6208972451355798336L;

	public SQLServer2008SpatialDialect() {
		super();

		// From Hibernate 4
		registerColumnType( Types.DATE, "date" );
		registerColumnType( Types.TIME, "time" );
		registerColumnType( Types.TIMESTAMP, "datetime2" );
	}
}
