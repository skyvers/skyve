package org.skyve.persistence;

import java.util.List;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Result-fetching contract for queries that return multiple columns as {@code Object[]} rows.
 *
 * <p>Use tuple queries when selecting two or more columns of different types from
 * a projected query. Column values are accessed by positional index in the returned
 * array, in the same order as the projections were added to the query.
 *
 * <p>The four methods follow the standard Skyve result-fetching convention:
 * <ul>
 *   <li>{@link #tupleResults()} — returns all rows as a list.
 *   <li>{@link #tupleResult()} — returns the first row or {@code null}.
 *   <li>{@link #retrieveTuple()} — returns exactly one row or throws.
 *   <li>{@link #tupleIterable()} — streams rows lazily; must be closed after use.
 * </ul>
 */
public interface TupleQuery {
	/**
	 * Returns all rows as {@code Object[]} arrays.
	 *
	 * @return a mutable list of rows; never {@code null}, may be empty
	 */
	@Nonnull List<Object[]> tupleResults();

	/**
	 * Returns the first row as an {@code Object[]} array, or {@code null} if empty.
	 *
	 * @return the first row, or {@code null}
	 */
	@Nullable Object[] tupleResult();

	/**
	 * Returns exactly one row; throws if there are zero or more than one results.
	 *
	 * @return the single row; never {@code null}
	 * @throws org.skyve.domain.messages.NoResultsException   if no rows are returned
	 * @throws org.skyve.domain.messages.ManyResultsException if more than one row is returned
	 */
	@Nonnull Object[] retrieveTuple();

	/**
	 * Returns a streaming {@link AutoClosingIterable} over all rows.
	 * Must be used in a try-with-resources block to guarantee cursor closure.
	 *
	 * @return a lazy cursor; never {@code null}
	 */
	@Nonnull AutoClosingIterable<Object[]> tupleIterable();
}
