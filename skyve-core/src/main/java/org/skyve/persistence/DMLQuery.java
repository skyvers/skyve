package org.skyve.persistence;

/**
 * Execution interface for data-manipulation queries (INSERT, UPDATE, DELETE).
 *
 * <p>Implemented by {@link BizQL} and {@link SQL} when those queries perform
 * data modification rather than data retrieval.
 */
public interface DMLQuery {
	/**
	 * Executes the data-modification statement and returns the number of affected rows.
	 *
	 * <p>Must be called within an active transaction. Changes are written to the
	 * database but are not committed until the surrounding transaction commits.
	 *
	 * @return the number of rows affected by the statement
	 */
	int execute();
}
