package org.skyve.metadata;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Declares an ordering of a binding ascending or descending.
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
