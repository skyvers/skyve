package org.skyve.dataaccess.sql;

import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.SQL;

public interface SQLDataAccess extends AutoCloseable {
	public SQL newSQL(String moduleName, String documentName, String query);

	public SQL newNamedSQL(String moduleName, String documentName, String queryName);
	
	public SQL newSQL(Document document, String query);

	public SQL newNamedSQL(Document document, String queryName);
	
	public SQL newSQL(String query);
	
	public SQL newNamedSQL(String moduleName, String queryName);
}
