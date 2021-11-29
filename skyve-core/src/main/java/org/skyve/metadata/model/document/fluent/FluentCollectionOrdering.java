package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.CollectionImpl.OrderingImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Collection.Ordering;

public class FluentCollectionOrdering {
	private OrderingImpl ordering = new OrderingImpl();
	
	public FluentCollectionOrdering() {
		// nothing to see
	}
	
	public FluentCollectionOrdering(Ordering ordering) {
		by(ordering.getBy());
		sort(ordering.getSort());
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
