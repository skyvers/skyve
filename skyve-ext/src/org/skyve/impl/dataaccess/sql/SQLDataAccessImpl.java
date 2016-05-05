package org.skyve.impl.dataaccess.sql;

import java.sql.Connection;

import javax.naming.InitialContext;
import javax.sql.DataSource;

import org.hibernatespatial.AbstractDBGeometryType;
import org.hibernatespatial.SpatialDialect;
import org.skyve.CORE;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.SQL;

public class SQLDataAccessImpl implements SQLDataAccess {
	private String dataSourceName; // to get a connection from
	private String dialectClassName; // to construct SQL from
	private AbstractDBGeometryType geometryUserType = null; // this is only created when we come across a geometry

	private Connection connection;
	
	public SQLDataAccessImpl() {
		this(UtilImpl.DATASOURCE);
	}
	
	public SQLDataAccessImpl(String dataSourceName) {
		this(dataSourceName, UtilImpl.DIALECT);
	}
	
	public SQLDataAccessImpl(String dataSourceName, String dialectClassName) {
		this.dataSourceName = dataSourceName;
		this.dialectClassName = dialectClassName;
	}
		
	Connection getConnection() throws Exception {
		if (connection == null) {
			InitialContext context = new InitialContext();
			DataSource dataSource = (DataSource) context.lookup(dataSourceName);
			connection = dataSource.getConnection();
		}
		
		return connection;
	}
	
	AbstractDBGeometryType getGeometryUserType() throws Exception {
		if (geometryUserType == null) {
			SpatialDialect dialect = (SpatialDialect) Class.forName(dialectClassName).newInstance();
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
	public SQL newSQL(String moduleName, String documentName, String query)
	throws MetaDataException {
		return new SQLDataAccessSQL(moduleName, documentName, query, this);
	}
	
	@Override
	public SQL newNamedSQL(String moduleName, String documentName, String queryName)
	throws MetaDataException {
		Module module = CORE.getUser().getCustomer().getModule(moduleName);
		return new SQLDataAccessSQL(moduleName, documentName, module.getSQL(queryName).getQuery(), this);
	}
	
	@Override
	public SQL newSQL(Document document, String query) {
		return new SQLDataAccessSQL(document, query, this);
	}

	@Override
	public SQL newNamedSQL(Document document, String queryName) 
	throws MetaDataException {
		Module module = CORE.getUser().getCustomer().getModule(document.getOwningModuleName());
		return new SQLDataAccessSQL(document, module.getSQL(queryName).getQuery(), this);
	}

	@Override
	public SQL newSQL(String query) {
		return new SQLDataAccessSQL(query, this);
	}

	@Override
	public SQL newNamedSQL(String moduleName, String queryName)
	throws MetaDataException {
		Module module = CORE.getUser().getCustomer().getModule(moduleName);
		return new SQLDataAccessSQL(module.getSQL(queryName).getQuery(), this);
	}
}
