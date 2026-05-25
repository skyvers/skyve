package org.skyve.persistence;

import jakarta.annotation.Nonnull;

/**
 * Pagination contract for queries that support result-set windowing.
 *
 * <p>Both {@link #setFirstResult(int)} and {@link #setMaxResults(int)} must be set
 * together to enable pagination. When neither is set the full result set is returned.
 */
public interface PagedQuery {
	/**
	 * Sets the zero-based index of the first result to retrieve.
	 *
	 * @param first zero-based offset of the first row; must be &ge; 0
	 * @return this query for fluent chaining
	 */
	public @Nonnull PagedQuery setFirstResult(int first);

	/**
	 * Limits the number of results returned.
	 *
	 * @param max the maximum number of rows to return; must be &gt; 0
	 * @return this query for fluent chaining
	 */
	public @Nonnull PagedQuery setMaxResults(int max);
}
