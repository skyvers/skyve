package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.OrderingImpl;
import org.skyve.metadata.Ordering;
import org.skyve.metadata.SortDirection;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides a fluent builder for FluentCollectionOrdering metadata.
 */
public class FluentCollectionOrdering {
	private OrderingImpl ordering = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentCollectionOrdering() {
		ordering = new OrderingImpl();
	}
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentCollectionOrdering(@Nonnull OrderingImpl ordering) {
		this.ordering = ordering;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentCollectionOrdering from(@SuppressWarnings("hiding") @Nonnull Ordering ordering) {
		by(ordering.getBy());
		sort(ordering.getSort());
		return this;
	}	
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentCollectionOrdering by(@Nonnull String by) {
		ordering.setBy(by);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentCollectionOrdering sort(@Nullable SortDirection sort) {
		ordering.setSort(sort);
		return this;
	}
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public @Nonnull OrderingImpl get() {
		return ordering;
	}
}
