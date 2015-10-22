package org.skyve.wildcat.dataaccess.sql;

import java.sql.Connection;

import javax.naming.InitialContext;
import javax.sql.DataSource;

import org.hibernate.usertype.UserType;
import org.hibernatespatial.SpatialDialect;
import org.skyve.CORE;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.SQL;
import org.skyve.wildcat.util.UtilImpl;

public class SQLDataAccess implements AutoCloseable {
	private String dataSourceName; // to get a connection from
	private String dialectClassName; // to construct SQL from
	private UserType geometryUserType = null; // this is only created when we come across a geometry

	private Connection connection;
	
	public SQLDataAccess() {
		this(UtilImpl.DATASOURCE);
	}
	
	public SQLDataAccess(String dataSourceName) {
		this(dataSourceName, UtilImpl.DIALECT);
	}
	
	public SQLDataAccess(String dataSourceName, String dialectClassName) {
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
	
	UserType getGeometryUserType() throws Exception {
		if (geometryUserType == null) {
			SpatialDialect dialect = (SpatialDialect) Class.forName(dialectClassName).newInstance();
			geometryUserType = dialect.getGeometryUserType();
		}
		
		return geometryUserType;
	}
	
	@Override
	public void close() throws Exception {
		if ((connection != null) && (! connection.isClosed())) {
			connection.close();
		}
	}
	
	public SQL newSQL(String moduleName, String documentName, String query)
	throws MetaDataException {
		return new SQLDataAccessSQL(moduleName, documentName, query, this);
	}
	
	public SQL newNamedSQL(String moduleName, String documentName, String queryName)
	throws MetaDataException {
		Module module = CORE.getUser().getCustomer().getModule(moduleName);
		return new SQLDataAccessSQL(moduleName, documentName, module.getSQL(queryName).getQuery(), this);
	}
	
	public SQL newSQL(Document document, String query) {
		return new SQLDataAccessSQL(document, query, this);
	}

	public SQL newNamedSQL(Document document, String queryName) 
	throws MetaDataException {
		Module module = CORE.getUser().getCustomer().getModule(document.getOwningModuleName());
		return new SQLDataAccessSQL(document, module.getSQL(queryName).getQuery(), this);
	}

	public SQL newSQL(String query) {
		return new SQLDataAccessSQL(query, this);
	}

	public SQL newNamedSQL(String moduleName, String queryName)
	throws MetaDataException {
		Module module = CORE.getUser().getCustomer().getModule(moduleName);
		return new SQLDataAccessSQL(module.getSQL(queryName).getQuery(), this);
	}
}
