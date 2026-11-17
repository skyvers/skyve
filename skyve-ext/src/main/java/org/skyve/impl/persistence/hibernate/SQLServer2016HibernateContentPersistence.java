package org.skyve.impl.persistence.hibernate;

import java.sql.Connection;
import java.sql.SQLException;

import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

/**
 * Configures {@link HibernateContentPersistence} for SQL Server 2016 snapshot-style transactions.
 *
 * <p>Side effects: the constructor updates the current JDBC connection isolation level to
 * SQL Server's snapshot mode when it is not already configured; loading the class logs a warning
 * (once per class loader, at the first instance).
 *
 * <p>Warning: at SNAPSHOT isolation Skyve's unique constraints and document numbers are not
 * enforced between concurrent transactions, so concurrent saves can store duplicate values and
 * take duplicate document numbers; Skyve needs READ COMMITTED. See
 * {@link org.skyve.impl.domain.number.AbstractDocumentNumberGenerator}.
 */
public class SQLServer2016HibernateContentPersistence extends HibernateContentPersistence {
	private static final long serialVersionUID = 3977869075169663549L;
	
	/**
	 * SQL Server-specific constant for snapshot isolation.
	 */
	private static final int TRANSACTION_SNAPSHOT = Connection.TRANSACTION_READ_COMMITTED + 4094;

	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(SQLServer2016HibernateContentPersistence.class);

	static {
		LOGGER.warn(isolationWarning(TRANSACTION_SNAPSHOT));
	}
	
	/**
	 * Creates the persistence instance and ensures the active connection uses SQL Server snapshot isolation.
	 *
	 * @throws SQLException if the connection cannot be inspected or the isolation level cannot be updated
	 */
	public SQLServer2016HibernateContentPersistence() throws SQLException {
		@SuppressWarnings("resource")
		Connection c = getConnection();
		if (c.getTransactionIsolation() != TRANSACTION_SNAPSHOT) {
			c.setTransactionIsolation(TRANSACTION_SNAPSHOT);
		}
	}
}
