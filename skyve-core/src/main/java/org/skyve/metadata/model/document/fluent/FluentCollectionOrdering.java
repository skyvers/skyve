package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.OrderingImpl;
import org.skyve.metadata.Ordering;
import org.skyve.metadata.SortDirection;

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
	public FluentCollectionOrdering(OrderingImpl ordering) {
		this.ordering = ordering;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentCollectionOrdering from(@SuppressWarnings("hiding") Ordering ordering) {
		by(ordering.getBy());
		sort(ordering.getSort());
		return this;
	}	
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCollectionOrdering by(String by) {
		ordering.setBy(by);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCollectionOrdering sort(SortDirection sort) {
		ordering.setSort(sort);
		return this;
	}
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public OrderingImpl get() {
		return ordering;
	}
}
