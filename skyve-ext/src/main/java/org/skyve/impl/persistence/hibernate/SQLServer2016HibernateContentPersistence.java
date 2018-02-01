package org.skyve.impl.persistence.hibernate;

import java.sql.Connection;
import java.sql.SQLException;

/**
 * In SQL Server 2016, the connection must be in a specific isolation mode (TRANSACTION_SNAPSHOT)
 * work like a real database and not a toy.
 * The below sets the underlying connection in this mode.
 * @author mike
 *
 */
public class SQLServer2016HibernateContentPersistence extends HibernateContentPersistence {
	private static final long serialVersionUID = 3977869075169663549L;
	
	/**
	 * Microsofts non-standard isolation level.
	 */
	private static final int TRANSACTION_SNAPSHOT = Connection.TRANSACTION_READ_COMMITTED + 4094;
	
	public SQLServer2016HibernateContentPersistence() throws SQLException {
		@SuppressWarnings("resource")
		Connection c = getConnection();
		if (c.getTransactionIsolation() != TRANSACTION_SNAPSHOT) {
			c.setTransactionIsolation(TRANSACTION_SNAPSHOT);
		}
	}
}
