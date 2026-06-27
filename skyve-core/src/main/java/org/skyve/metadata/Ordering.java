package org.skyve.metadata;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A single sort specification combining an attribute binding with a sort direction.
 *
 * <p>Multiple {@code Ordering} instances are used together to express multi-column
 * ORDER BY clauses in queries and list views. When {@link #getSort()} returns
 * {@code null} the convention is ascending order.
 *
 * @see SortDirection
 * @see org.skyve.persistence.DocumentQuery#addBoundOrdering(String, SortDirection)
 */
public interface Ordering extends SerializableMetaData {
	/**
	 * The binding to order by
	 * @return	The binding.
	 */
	public @Nonnull String getBy();
	
	/**
	 * Ascending or descending (null = AScending)
	 * @return
	 */
	public @Nullable SortDirection getSort();
}
