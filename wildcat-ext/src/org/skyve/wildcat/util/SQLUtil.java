package org.skyve.wildcat.util;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Date;
import java.util.List;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import org.apache.commons.beanutils.DynaBean;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.TimeOnly;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

public class SQLUtil {
	public static final String SQL_DATE_ONLY_FORMAT = "yyyy-MM-dd";
	public static final String SQL_TIME_ONLY_FORMAT = "HH:mm:ss";
	public static final String SQL_DATE_TIME_FORMAT = "yyyy-MM-dd HH:mm:ss";

	// TODO Make private
	public static final ThreadLocal<Connection> threadLocalConnection = new ThreadLocal<Connection>() {
		@Override
		protected synchronized Connection initialValue() throws IllegalStateException {
			Connection connection = getAPooledConnection();
			set(connection);
			return connection;
		}
	};

	private SQLUtil() {
		// no implementation
	}

	public static Connection getAPooledConnection()
	throws IllegalStateException {
		Connection result = null;
		try {
			InitialContext ctx = new InitialContext();
			DataSource ds = (DataSource) ctx.lookup(UtilImpl.DATASOURCE);
			result = ds.getConnection();
			result.setAutoCommit(false);
		}
		catch (SQLException e) {
			throw new IllegalStateException("Could not get a database connection", e);
		}
		catch (NamingException e) {
			throw new IllegalStateException("Could not find the JDBC connection pool", e);
		}

		return result;
	}

	/**
	 * 
	 * @param tableNameDotFieldName
	 * @param document
	 * @param searchValue
	 * @param customerId This can be <code>null</code> if the document is a metadata document.
	 * @return
	 * @throws DomainException
	 */
	public static List<DynaBean> retrieveSuggestions(String fieldName, 
														Document document, 
														String searchValue, 
														User user)
	throws DomainException, MetaDataException {
		StringBuilder sql = new StringBuilder(64);

		if (! document.getAttribute(fieldName).isPersistent()) {
			throw new IllegalStateException("Field " + fieldName + " in document " + document.getName() + "is not persistent.");
		}

		sql.append("select ");
		sql.append(fieldName);
		sql.append(", count(1) as SuggestionOccurences from ");
		sql.append(document.getPersistent().getPersistentIdentifier());
		sql.append(" where ").append(Bean.CUSTOMER_NAME).append(" = '").append(user.getCustomer().getName());
		sql.append("' and ");
		sql.append(fieldName).append(" like '");
		sql.append(searchValue).append("%' group by ").append(fieldName);

		return retrieveListForSQL(document.getOwningModuleName(), 
									document.getName(), 
									sql.toString(), 
									user, 
									false, 
									false);
	}

	/**
	 * 
	 * @param document
	 * @param id
	 * @param customerId This can be <code>null</code> if document is a metadata document.
	 * @return
	 * @throws DomainException
	 */
	public static DynaBean retrieveBean(Document document, 
											String id, 
											User user, 
											boolean forUpdate, 
											@SuppressWarnings("unused") boolean withReadLock)
	throws DomainException, MetaDataException {
		assert (id != null);

		StringBuilder sql = new StringBuilder(32);
		sql.append("select * from ").append(document.getPersistent().getPersistentIdentifier());
		sql.append(" where ").append(Bean.CUSTOMER_NAME).append(" = '").append(user.getCustomer().getName());
		sql.append("' and ");
		sql.append(Bean.DOCUMENT_ID).append(" = '").append(id).append('\'');

		return retrieveBeanForSQL(document.getOwningModuleName(), 
									document.getName(), 
									sql.toString(), 
									user, 
									forUpdate,
									false);
	}

	static String generateSQLFragment(Object value) {
		StringBuilder result = new StringBuilder(32);

		if (value == null) {
			result.append("null");
		}
		else if (value instanceof String) {
			result.append('\'');
			String valueString = (String) value;
			result.append(valueString.replace("'", "''"));
			result.append('\'');
		}
		else if (value instanceof DateOnly) {
			result.append("DATE('");
			result.append(ThreadSafeFactory.getDateFormat(SQL_DATE_ONLY_FORMAT).format((Date) value));
			result.append("')");
		}
		else if (value instanceof TimeOnly) {
			result.append("TIME('");
			result.append(ThreadSafeFactory.getDateFormat(SQL_TIME_ONLY_FORMAT).format((Date) value));
			result.append("')");
		}
		else if (value instanceof Date) {
			result.append("TIMESTAMP('");
			result.append(ThreadSafeFactory.getDateFormat(SQL_DATE_TIME_FORMAT).format((Date) value));
			result.append("')");
		}
		else if (value instanceof OptimisticLock) {
			result.append('\'').append(((OptimisticLock) value).toString()).append('\'');
		}
		else {
			result.append(value);
		}

		return result.toString();
	}

/*
	private static void execute(String sql)
	throws SQLException
	{
		Connection connection = threadLocalConnection.get();
		Statement statement = null;
		try
		{
			statement = connection.createStatement();

			if (Util.QUERY_TRACE) Util.LOGGER.info(sql + " executed on thread " + Thread.currentThread() + ", connection = " + connection);
			statement.execute(sql);
		}
		finally
		{
			if (statement != null)
			{
				statement.close();
			}
		}
	}
*/

	public static void disconnect(Connection connection)
	throws DomainException {
		try {
			if ((connection != null) && (! connection.isClosed())) {
				connection.close();
			}
		}
		catch (SQLException e) {
			throw new DomainException(e);
		}
	}

	@SuppressWarnings("resource")
	public static void commit(boolean disconnect)
	throws DomainException {
		Connection connection = threadLocalConnection.get();
		if (connection != null) {
			try {
				if (! connection.getAutoCommit()) {
					connection.commit();
				}
				else {
					System.err.println("COMMIT - Connection is on autocommit - THAT IS GAY");
				}
			}
			catch (SQLException e) {
				throw new DomainException(e);
			}
			finally {
				if (disconnect) {
					threadLocalConnection.remove();
					disconnect(connection);
				}
			}
		}
	}

	@SuppressWarnings("resource")
	public static void rollback() throws DomainException {
		Connection connection = threadLocalConnection.get();
		if (connection != null) {
			try {
				if (! connection.getAutoCommit()) {
					connection.rollback();
				}
				else {
					System.err.println("ROLLBACK - Connection is on autocommit - THAT IS GAY");
				}
			}
			catch (SQLException e) {
				throw new DomainException(e);
			}
			finally {
				threadLocalConnection.remove();
				disconnect(connection);
			}
		}
	}

	// TODO Make this method package private later
	@SuppressWarnings("resource")
	public static List<DynaBean> retrieveListForSQL(String moduleName,
														String documentName,
														String sql,
														User user,
														boolean forUpdate,
														boolean withReadLock)
	throws DomainException {
		assert (sql != null);
		String newSQL = sql;
		
		if (forUpdate) {
			newSQL += " FOR UPDATE";
		}
		else if (withReadLock) {
			newSQL += " LOCK IN SHARE MODE";
		}

		SQLRowSetDynaClass rsdc = null;
		Connection connection = threadLocalConnection.get();
		try {
			Customer customer = null;
			Module module = null;
			if ((user != null) && (moduleName != null)) {
				customer = user.getCustomer();
				module = customer.getModule(moduleName);
			}
			if (UtilImpl.QUERY_TRACE) UtilImpl.LOGGER.info(newSQL + " executed on thread " + Thread.currentThread() + ", connection = " + connection);
			Statement statement = connection.createStatement();
			ResultSet resultSet = statement.executeQuery(newSQL);

			try {
				rsdc = new SQLRowSetDynaClass(customer, module, documentName, resultSet);
			}
			finally {
				if (resultSet != null) {
					resultSet.close();
				}
				statement.close();
			}
		}
		catch (SQLException e) {
			throw new DomainException(e);
		}
		catch (Exception e) {
			throw new DomainException(e);
		}

		return rsdc.getRows();
	}

	private static DynaBean retrieveBeanForSQL(String moduleName,
												String documentName,
												String sql,
												User user,
												boolean forUpdate,
												boolean withReadLock)
	throws DomainException {
		DynaBean result = null;

		List<DynaBean> list = retrieveListForSQL(moduleName, documentName, sql, user, forUpdate, withReadLock);
		if (! list.isEmpty()) {
			result = list.get(0);
		}

		return result;
	}
}
