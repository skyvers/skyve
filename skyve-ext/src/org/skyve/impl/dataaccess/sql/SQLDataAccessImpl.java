package org.skyve.impl.dataaccess.sql;

import java.sql.Connection;

import org.hibernatespatial.AbstractDBGeometryType;
import org.hibernatespatial.SpatialDialect;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DataStore;
import org.skyve.persistence.SQL;

public class SQLDataAccessImpl implements SQLDataAccess {
	private DataStore dataStore; // to get a connection and construct SQL from
	private AbstractDBGeometryType geometryUserType = null; // this is only created when we come across a geometry

	private Connection connection;
	
	public SQLDataAccessImpl(DataStore dataStore) {
		this.dataStore = dataStore;
	}
	
	Connection getConnection() throws Exception {
		if (connection == null) {
			connection = EXT.getDataStoreConnection(dataStore);
		}
		
		return connection;
	}
	
	AbstractDBGeometryType getGeometryUserType() throws Exception {
		if (geometryUserType == null) {
			SpatialDialect dialect = (SpatialDialect) Class.forName(dataStore.getDialectClassName()).newInstance();
			geometryUserType = (AbstractDBGeometryType) dialect.getGeometryUserType();
		}
		
		return geometryUserType;
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
		return new SQLDataAccessSQL(moduleName, documentName, module.getSQL(queryName).getQuery(), this);
	}
	
	@Override
	public SQL newSQL(Document document, String query) {
		return new SQLDataAccessSQL(document, query, this);
	}

	@Override
	public SQL newNamedSQL(Document document, String queryName) {
		Module module = CORE.getUser().getCustomer().getModule(document.getOwningModuleName());
		return new SQLDataAccessSQL(document, module.getSQL(queryName).getQuery(), this);
	}

	@Override
	public SQL newSQL(String query) {
		return new SQLDataAccessSQL(query, this);
	}

	@Override
	public SQL newNamedSQL(String moduleName, String queryName) {
		Module module = CORE.getUser().getCustomer().getModule(moduleName);
		return new SQLDataAccessSQL(module.getSQL(queryName).getQuery(), this);
	}
}
