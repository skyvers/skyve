package org.skyve.dataaccess.sql;

import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.SQL;

/**
 * Provides direct SQL data access capabilities for executing raw SQL queries
 * outside of the standard Skyve persistence layer.
 * <p>
 * This interface extends {@link AutoCloseable} to ensure proper resource cleanup
 * when used in try-with-resources statements. It offers methods to create SQL
 * query objects with various levels of document binding, from fully qualified
 * module/document references to standalone queries.
 * </p>
 * <p>
 * Implementations of this interface manage their own database connections and
 * transactions, providing explicit {@link #commit()} and {@link #rollback()}
 * methods for transaction control.
 * </p>
 * 
 * @see SQL
 * @see Document
 */
public interface SQLDataAccess extends AutoCloseable {
	
	/**
	 * Creates a new SQL query bound to a specific document by module and document name.
	 * 
	 * @param moduleName the name of the module containing the document
	 * @param documentName the name of the document to bind the query to
	 * @param query the SQL query string to execute
	 * @return a new {@link SQL} instance configured with the specified query and document binding
	 */
	public SQL newSQL(String moduleName, String documentName, String query);

	/**
	 * Creates a new SQL query from a named query definition bound to a specific document.
	 * 
	 * @param moduleName the name of the module containing the document
	 * @param documentName the name of the document to bind the query to
	 * @param queryName the name of the predefined query to use
	 * @return a new {@link SQL} instance configured with the named query and document binding
	 */
	public SQL newNamedSQL(String moduleName, String documentName, String queryName);
	
	/**
	 * Creates a new SQL query bound to a specific document.
	 * 
	 * @param document the document to bind the query to
	 * @param query the SQL query string to execute
	 * @return a new {@link SQL} instance configured with the specified query and document binding
	 */
	public SQL newSQL(Document document, String query);

	/**
	 * Creates a new SQL query from a named query definition bound to a specific document.
	 * 
	 * @param document the document to bind the query to
	 * @param queryName the name of the predefined query to use
	 * @return a new {@link SQL} instance configured with the named query and document binding
	 */
	public SQL newNamedSQL(Document document, String queryName);
	
	/**
	 * Creates a new standalone SQL query without document binding.
	 * <p>
	 * This method is suitable for queries that don't map to a specific Skyve document,
	 * such as aggregate queries or queries against non-Skyve tables.
	 * </p>
	 * 
	 * @param query the SQL query string to execute
	 * @return a new {@link SQL} instance configured with the specified query
	 */
	public SQL newSQL(String query);
	
	/**
	 * Creates a new SQL query from a named query definition within a module.
	 * 
	 * @param moduleName the name of the module containing the named query
	 * @param queryName the name of the predefined query to use
	 * @return a new {@link SQL} instance configured with the named query
	 */
	public SQL newNamedSQL(String moduleName, String queryName);
	
	/**
	 * Commits the current transaction.
	 * <p>
	 * All changes made since the last commit or rollback will be persisted
	 * to the database.
	 * </p>
	 */
	public void commit();
	
	/**
	 * Rolls back the current transaction.
	 * <p>
	 * All changes made since the last commit or rollback will be discarded.
	 * </p>
	 */
	public void rollback();
}