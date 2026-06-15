package org.skyve.persistence;

import java.util.List;

import org.skyve.domain.Bean;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Result-fetching contract for queries that return typed {@link Bean} instances.
 *
 * <p>The four methods follow the standard Skyve result-fetching convention:
 * <ul>
 *   <li>{@link #beanResults()} — returns all matching beans as a list.
 *   <li>{@link #beanResult()} — returns the first bean or {@code null}.
 *   <li>{@link #retrieveBean()} — returns exactly one bean or throws.
 *   <li>{@link #beanIterable()} — streams beans lazily from a cursor;
 *       must be closed after use.
 * </ul>
 *
 * <p>The generic type {@code T} is inferred from the calling context.
 * For type-safe retrieval the calling code should declare the expected type;
 * an unchecked cast is performed internally.
 */
public interface BeanQuery {
	/**
	 * Returns a list of all the beans which match this query.
	 * 
	 * @see org.skyve.impl.persistence.AbstractBizQL#beanResults()
	 * @return The list of beans that match the query
	 */
	@Nonnull <T extends Bean> List<T> beanResults();
	
	/**
     * Returns the first bean result from this query, or null if there are no
     * results.
     * 
     * @see org.skyve.impl.persistence.AbstractQuery#returnOneResult(List)
     * @return The first bean that matches the query, or null
     */
	@Nullable <T extends Bean> T beanResult();
	
	/**
     * Returns exactly one bean result from this query; or throws a
     * NoResultsException or ManyResultsException if there are no or too many results, 
	 * as appropriate.
     * 
     * @see org.skyve.impl.persistence.AbstractQuery#assertOneResult(List)
     * @return One result bean
     */
	@Nonnull <T extends Bean> T retrieveBean();

	/**
	 * Returns an iterable for all the beans which match this query.
	 * 
	 * @return The iterarable
	 */
	@Nonnull <T extends Bean> AutoClosingIterable<T> beanIterable();
}
