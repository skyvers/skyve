package org.skyve.persistence;

import java.util.List;

import org.skyve.domain.Bean;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Result-fetching contract for queries that return beans populated from projected
 * column aliases.
 *
 * <p>Projected results are {@link org.skyve.domain.Bean} instances (typically
 * {@link org.skyve.domain.DynamicBean}) where each projected binding or expression
 * alias maps to a bean attribute. Use projected queries when you need a subset of
 * attributes or a computed view rather than the full entity.
 *
 * <p>The four methods follow the standard Skyve result-fetching convention:
 * <ul>
 *   <li>{@link #projectedResults()} — returns all matching rows as a list.
 *   <li>{@link #projectedResult()} — returns the first row or {@code null}.
 *   <li>{@link #retrieveProjected()} — returns exactly one row or throws.
 *   <li>{@link #projectedIterable()} — streams rows lazily; must be closed after use.
 * </ul>
 */
public interface ProjectedQuery {
	/**
	 * Returns all matching rows as projected bean instances.
	 *
	 * @param <T> the expected bean type
	 * @return a mutable list of projected beans; never {@code null}, may be empty
	 */
	@Nonnull <T extends Bean> List<T> projectedResults();

	/**
	 * Returns the first projected bean, or {@code null} if the result set is empty.
	 *
	 * @param <T> the expected bean type
	 * @return the first row, or {@code null}
	 */
	@Nullable <T extends Bean> T projectedResult();

	/**
	 * Returns exactly one projected bean; throws if there are zero or more than one results.
	 *
	 * @param <T> the expected bean type
	 * @return the single matching bean; never {@code null}
	 * @throws org.skyve.domain.messages.NoResultsException   if no rows are returned
	 * @throws org.skyve.domain.messages.ManyResultsException if more than one row is returned
	 */
	@Nonnull <T extends Bean> T retrieveProjected();

	/**
	 * Returns a streaming {@link AutoClosingIterable} over all projected rows.
	 * Must be used in a try-with-resources block to guarantee cursor closure.
	 *
	 * @param <T> the expected bean type
	 * @return a lazy cursor; never {@code null}
	 */
	@Nonnull <T extends Bean> AutoClosingIterable<T> projectedIterable();
}
