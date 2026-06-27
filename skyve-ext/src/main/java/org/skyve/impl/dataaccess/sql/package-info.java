/**
 * Direct SQL data access implementation for Skyve.
 *
 * <p>{@code SQLDataAccessImpl} implements {@link org.skyve.persistence.SQLDataAccess}
 * using a JDBC connection to a named {@link org.skyve.persistence.DataStore}.
 * {@code SQLIterable} provides auto-closing iteration over SQL result sets.
 * {@code StreamableConnection} wraps a {@link java.sql.Connection} to support
 * streaming result-set consumption.
 */
package org.skyve.impl.dataaccess.sql;
