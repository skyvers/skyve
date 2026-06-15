package org.skyve.persistence;

import java.util.List;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Result-fetching contract for queries that return a single column of scalar values.
 *
 * <p>Use scalar queries when selecting a single aggregate, a single identifier column,
 * or any other single-value expression. The column type is mapped to the supplied
 * Java type {@code T}.
 *
 * <p>The four methods follow the standard Skyve result-fetching convention:
 * <ul>
 *   <li>{@link #scalarResults(Class)} — returns all values as a list.
 *   <li>{@link #scalarResult(Class)} — returns the first value or {@code null}.
 *   <li>{@link #retrieveScalar(Class)} — returns exactly one value or throws.
 *   <li>{@link #scalarIterable(Class)} — streams values lazily; must be closed after use.
 * </ul>
 */
public interface ScalarQuery {
	/**
	 * Returns all scalar values as a list of the given type.
	 *
	 * @param <T>  the Java type to map the column value to
	 * @param type the class of the expected return type; must not be {@code null}
	 * @return a mutable list of values; never {@code null}, may be empty
	 */
	@Nonnull <T extends Object> List<T> scalarResults(Class<T> type);

	/**
	 * Returns the first scalar value, or {@code null} if the result set is empty.
	 *
	 * @param <T>  the Java type to map the column value to
	 * @param type the class of the expected return type; must not be {@code null}
	 * @return the first value, or {@code null}
	 */
	@Nullable <T extends Object> T scalarResult(Class<T> type);

	/**
	 * Returns exactly one scalar value; throws if there are zero or more than one results.
	 *
	 * @param <T>  the Java type to map the column value to
	 * @param type the class of the expected return type; must not be {@code null}
	 * @return the single scalar value; never {@code null}
	 * @throws org.skyve.domain.messages.NoResultsException   if no rows are returned
	 * @throws org.skyve.domain.messages.ManyResultsException if more than one row is returned
	 */
	@Nonnull <T extends Object> T retrieveScalar(Class<T> type);

	/**
	 * Returns a streaming {@link AutoClosingIterable} over all scalar values.
	 * Must be used in a try-with-resources block to guarantee cursor closure.
	 *
	 * @param <T>  the Java type to map the column value to
	 * @param type the class of the expected return type; must not be {@code null}
	 * @return a lazy cursor; never {@code null}
	 */
	@Nonnull <T extends Object> AutoClosingIterable<T> scalarIterable(Class<T> type);
}
