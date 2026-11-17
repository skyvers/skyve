package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.impl.util.UtilImpl;

import util.AbstractH2Test;

/**
 * Tests the warning Skyve logs when the data store's connections do not use READ COMMITTED, the only
 * isolation level at which its unique constraints and document numbers hold between concurrent transactions.
 */
class IsolationWarningH2Test extends AbstractH2Test {
	@Test
	@SuppressWarnings("static-method")
	void readCommittedHasNoWarning() {
		assertNull(AbstractHibernatePersistence.isolationWarning(Connection.TRANSACTION_READ_COMMITTED));
	}

	@Test
	@SuppressWarnings("static-method")
	void everyOtherLevelIsNamedWithItsConsequenceAndTheRemedy() {
		Map<Integer, String> levels = Map.of(Integer.valueOf(Connection.TRANSACTION_NONE), "NONE",
												Integer.valueOf(Connection.TRANSACTION_READ_UNCOMMITTED), "READ UNCOMMITTED",
												Integer.valueOf(Connection.TRANSACTION_REPEATABLE_READ), "REPEATABLE READ",
												Integer.valueOf(Connection.TRANSACTION_SERIALIZABLE), "SERIALIZABLE",
												Integer.valueOf(AbstractHibernatePersistence.SQL_SERVER_TRANSACTION_SNAPSHOT), "SNAPSHOT",
												Integer.valueOf(99), "level 99");
		List<Executable> checks = new ArrayList<>(levels.size());
		for (Map.Entry<Integer, String> level : levels.entrySet()) {
			String warning = AbstractHibernatePersistence.isolationWarning(level.getKey().intValue());
			String name = level.getValue();
			checks.add(() -> {
				assertNotNull(warning, name);
				assertAll(name,
							() -> assertTrue(warning.contains("isolation level is " + name + ", not READ COMMITTED"), warning),
							() -> assertTrue(warning.contains("unique constraints and document numbers are not enforced between concurrent transactions"), warning),
							() -> assertTrue(warning.contains("<transaction-isolation>TRANSACTION_READ_COMMITTED</transaction-isolation>"), warning));
			});
		}
		assertAll(checks);
	}

	/**
	 * The test data store, H2, uses READ COMMITTED, so the start-up check is silent; switched to
	 * REPEATABLE READ, the same connection gets the warning.
	 */
	@Test
	@SuppressWarnings("static-method")
	void aConnectionIsCheckedAtItsActualLevel() throws Exception {
		try (Connection connection = DriverManager.getConnection(UtilImpl.DATA_STORE.getJdbcUrl(), UtilImpl.DATA_STORE.getUserName(), UtilImpl.DATA_STORE.getPassword())) {
			assertNull(AbstractHibernatePersistence.warnUnlessReadCommitted(connection), "H2 is READ COMMITTED");
			connection.setTransactionIsolation(Connection.TRANSACTION_REPEATABLE_READ);
			String warning = AbstractHibernatePersistence.warnUnlessReadCommitted(connection);
			assertNotNull(warning);
			assertTrue(warning.contains("REPEATABLE READ"), warning);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void anUnreadableLevelIsNotAFailure() throws Exception {
		Connection closed;
		try (Connection connection = DriverManager.getConnection(UtilImpl.DATA_STORE.getJdbcUrl(), UtilImpl.DATA_STORE.getUserName(), UtilImpl.DATA_STORE.getPassword())) {
			closed = connection;
		}
		assertNull(AbstractHibernatePersistence.warnUnlessReadCommitted(closed));
	}

	/**
	 * Loading the SNAPSHOT persistence logs the warning (its static initialiser runs before the first
	 * instance); H2 then rejects SQL Server's SNAPSHOT level.
	 */
	@Test
	@SuppressWarnings("static-method")
	void theSnapshotPersistenceIsLoadedWithItsWarning() {
		assertThrows(SQLException.class, SQLServer2016HibernateContentPersistence::new);
	}
}
