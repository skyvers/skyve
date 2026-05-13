package org.skyve.impl.dataaccess.sql;

import java.sql.Connection;
import java.sql.SQLException;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.persistence.hibernate.dialect.SkyveDialect;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.SQLDefinition;
import org.skyve.persistence.DataStore;
import org.skyve.persistence.SQL;

/**
 * Implementation of {@link SQLDataAccess} that provides direct SQL data access
 * using JDBC connections to a specified {@link DataStore}.
 * <p>
 * This class manages its own database connection lifecycle and supports
 * transaction control through {@link #commit()} and {@link #rollback()} methods.
 * When closed, any uncommitted changes are automatically committed.
 * </p>
 * <p>
 * The connection is lazily initialised on first use and reused for subsequent
 * SQL operations within the same instance.
 * </p>
 * 
 * @see SQLDataAccess
 * @see DataStore
 */
public class SQLDataAccessImpl implements SQLDataAccess {
	protected DataStore dataStore; // to get a connection and construct SQL from
	private SkyveDialect dialect = null; // this is only created when we come across a geometry

	private Connection connection;
	
	/**
	 * Constructs a new SQLDataAccessImpl for the specified data store.
	 * 
	 * @param dataStore the data store to connect to for SQL operations
	 */
	public SQLDataAccessImpl(DataStore dataStore) {
		this.dataStore = dataStore;
	}
	
	/**
	 * Returns the JDBC connection for this data access instance.
	 * <p>
	 * The connection is lazily initialised on first call and reused
	 * for subsequent calls. Auto-commit is disabled on the connection.
	 * </p>
	 * 
	 * @return the JDBC connection
	 */
	Connection getConnection() {
		if (connection == null) {
			connection = EXT.getDataStoreConnection(dataStore, true);
		}
		
		return connection;
	}
	
	/**
	 * Returns the Skyve dialect for the configured data store.
	 * <p>
	 * The dialect is lazily initialized on first call. It is used for
	 * handling database-specific features such as geometry types.
	 * </p>
	 * 
	 * @return the Skyve dialect for the data store
	 * @throws Exception if unable to instantiate the dialect
	 */
	SkyveDialect getDialect() throws Exception {
		if (dialect == null) {
			dialect = AbstractHibernatePersistence.getDialect(dataStore.getDialectClassName());
		}
		
		return dialect;
	}
	
	@Override
	public void close() throws Exception {
		if ((connection != null) && (! connection.isClosed())) {
			connection.close();
		}
	}
	
	@Override
	public SQL newSQL(String moduleName, String documentName, String query) {
		return new SQLDataAccessSQL(moduleName, documentName, query, this);
	}
	
	@Override
	public SQL newNamedSQL(String moduleName, String documentName, String queryName) {
		Module module = CORE.getUser().getCustomer().getModule(moduleName);
		SQLDefinition sql = module.getSQL(queryName);
		SQLDataAccessSQL result = new SQLDataAccessSQL(moduleName, documentName, sql.getQuery(), this);
		result.setTimeoutInSeconds(sql.getTimeoutInSeconds());
		return result;
	}
	
	@Override
	public SQL newSQL(Document document, String query) {
		return new SQLDataAccessSQL(document, query, this);
	}

	@Override
	public SQL newNamedSQL(Document document, String queryName) {
		Module module = CORE.getUser().getCustomer().getModule(document.getOwningModuleName());
		SQLDefinition sql = module.getSQL(queryName);
		SQLDataAccessSQL result = new SQLDataAccessSQL(document, sql.getQuery(), this);
		result.setTimeoutInSeconds(sql.getTimeoutInSeconds());
		return result;
	}

	@Override
	public SQL newSQL(String query) {
		return new SQLDataAccessSQL(query, this);
	}

	@Override
	public SQL newNamedSQL(String moduleName, String queryName) {
		Module module = CORE.getUser().getCustomer().getModule(moduleName);
		SQLDefinition sql = module.getSQL(queryName);
		SQLDataAccessSQL result = new SQLDataAccessSQL(sql.getQuery(), this);
		result.setTimeoutInSeconds(sql.getTimeoutInSeconds());
		return result;
	}
	
	@Override
	public void rollback() {
		try {
			if ((connection != null) && (! connection.isClosed())) {
				connection.rollback();
			}
		}
		catch (SQLException e) {
			throw new DomainException(e);
		}
	}
	
	@Override
	public void commit() {
		try {
			if ((connection != null) && (! connection.isClosed())) {
				connection.commit();
			}
		}
		catch (SQLException e) {
			throw new DomainException(e);
		}
	}
}