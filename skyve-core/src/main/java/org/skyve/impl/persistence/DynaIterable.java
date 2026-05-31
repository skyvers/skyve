package org.skyve.impl.persistence;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Iterator;

import org.apache.commons.beanutils.DynaBean;
import org.apache.commons.beanutils.ResultSetDynaClass;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.SkyveException;
import org.skyve.domain.messages.TimeoutException;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.impl.util.LoggingIteratorAdapter;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DataStore;

/**
 * A lazy, auto-closing {@link org.skyve.persistence.AutoClosingIterable} over a raw
 * SQL query result set, surfacing each row as an Apache Commons
 * {@link org.apache.commons.beanutils.DynaBean}.
 *
 * <p>The underlying JDBC {@link java.sql.PreparedStatement} and
 * {@link java.sql.ResultSet} are opened in the constructor and closed when
 * {@link #close()} is called or iteration is exhausted.
 *
 * <p>Side effects: opens a JDBC statement and result-set on the provided connection;
 * callers must ensure {@link #close()} is called (preferably via try-with-resources).
 */
public class DynaIterable implements AutoClosingIterable<DynaBean> {
	@SuppressWarnings("resource")
	private final NamedParameterPreparedStatement ps;
	@SuppressWarnings("resource")
	private final ResultSet rs;
	
	/**
	 * Executes the SQL query immediately and prepares lazy row iteration.
	 *
	 * <p>Side effects: opens a prepared statement and result set on the supplied
	 * connection.
	 *
	 * @param c JDBC connection used to create and execute the statement
	 * @param sql SQL query definition and bound parameters
	 * @param ds data-store timeout configuration
	 * @param dialect SQL dialect used for parameter conversion
	 */
	@SuppressWarnings("resource")
	public DynaIterable(Connection c, AbstractSQL sql, DataStore ds, SkyveDialect dialect) {
		try {
			ps = new NamedParameterPreparedStatement(c, sql.toQueryString());
			sql.prepareStatement(ps, ds, dialect);
			rs = ps.executeQuery();
		}
		catch (TimeoutException e) {
			throw e;
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	/**
	 * Returns an iterator over detached dynamic rows from the active result set.
	 *
	 * @return iterator over query rows as {@link DynaBean} instances
	 */
	@Override
	public Iterator<DynaBean> iterator() {
		try {
			final ResultSetDynaClass rsdc = new ResultSetDynaClass(rs, true, true);
			return new LoggingIteratorAdapter<>(rsdc.iterator());
		}
		catch (SQLException e) {
			throw new DomainException(e);
		}
	}

	/**
	 * Closes the underlying JDBC result set and prepared statement.
	 *
	 * @throws Exception if closing either resource fails
	 */
	@Override
	public void close() throws Exception {
		if ((rs != null) && (! rs.isClosed())) {
			rs.close();
		}
		if ((ps != null) && (! ps.isClosed())) {
			ps.close();
		}
	}
}
