package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.CollectionImpl.OrderingImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Collection.Ordering;

public class FluentCollectionOrdering {
	private OrderingImpl ordering = null;
	
	public FluentCollectionOrdering() {
		ordering = new OrderingImpl();
	}
	
	public FluentCollectionOrdering(OrderingImpl ordering) {
		this.ordering = ordering;
	}

	public FluentCollectionOrdering from(@SuppressWarnings("hiding") Ordering ordering) {
		by(ordering.getBy());
		sort(ordering.getSort());
		return this;
	}	
	
	public FluentCollectionOrdering by(String by) {
		ordering.setBy(by);
		return this;
	}
	
	public FluentCollectionOrdering sort(SortDirection sort) {
		ordering.setSort(sort);
		return this;
	}
	
	public OrderingImpl get() {
		return ordering;
	}
}
