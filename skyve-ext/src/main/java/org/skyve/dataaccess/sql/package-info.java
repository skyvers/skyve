/**
 * Direct SQL data access API for querying outside the Skyve persistence layer.
 *
 * <p>{@link org.skyve.dataaccess.sql.SQLDataAccess} wraps a raw JDBC connection with
 * Skyve-typed query builders ({@link org.skyve.persistence.SQL}) and explicit
 * transaction control. Obtain an instance via {@link org.skyve.EXT#newSQLDataAccess()}.
 * Always use it in a try-with-resources block; the connection is closed on
 * {@link AutoCloseable#close()}.
 *
 * @see org.skyve.dataaccess.sql.SQLDataAccess
 * @see org.skyve.persistence.SQL
 */
package org.skyve.dataaccess.sql;
