package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Collection.Ordering;
import org.skyve.metadata.model.document.UniqueConstraint;

public class FluentCollection extends FluentReference<FluentCollection> {
	private CollectionImpl collection = null;
	
	public FluentCollection() {
		collection = new CollectionImpl();
	}

	public FluentCollection(CollectionImpl collection) {
		this.collection = collection;
	}

	public FluentCollection from(@SuppressWarnings("hiding") Collection collection) {
		super.from(collection);
		type(collection.getType());
		ordered(Boolean.TRUE.equals(collection.getOrdered()));
		minCardinality(collection.getMinCardinality().intValue());
		Integer i = collection.getMaxCardinality();
		if (i != null) {
			maxCardinality(i.intValue());
		}
		ownerDatabaseIndex(! Boolean.FALSE.equals(collection.getOwnerDatabaseIndex()));
		elementDatabaseIndex(! Boolean.FALSE.equals(collection.getElementDatabaseIndex()));
		cacheName(collection.getCacheName());

		for (Ordering ordering : collection.getOrdering()) {
			addOrdering(new FluentCollectionOrdering().from(ordering));
		}
		
		for (UniqueConstraint constraint : collection.getUniqueConstraints()) {
			addUniqueConstraint(new FluentCollectionUniqueConstraint().from(constraint));
		}
		return this;
	}
	
	public FluentCollection type(CollectionType type) {
		collection.setType(type);
		return this;
	}
	
	public FluentCollection ordered(boolean ordered) {
		collection.setOrdered(ordered ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentCollection minCardinality(int minCardinality) {
		collection.setMinCardinality(Integer.valueOf(minCardinality));
		return this;
	}

	public FluentCollection maxCardinality(int maxCardinality) {
		collection.setMaxCardinality(Integer.valueOf(maxCardinality));
		return this;
	}

	public FluentCollection ownerDatabaseIndex(boolean ownerDatabaseIndex) {
		collection.setOwnerDatabaseIndex(ownerDatabaseIndex ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentCollection elementDatabaseIndex(boolean elementDatabaseIndex) {
		collection.setElementDatabaseIndex(elementDatabaseIndex ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentCollection cacheName(String cacheName) {
		collection.setCacheName(cacheName);
		return this;
	}
	
	public FluentCollection addOrdering(FluentCollectionOrdering ordering) {
		collection.getOrdering().add(ordering.get());
		return this;
	}

	public FluentCollection addUniqueConstraint(FluentCollectionUniqueConstraint constraint) {
		collection.getUniqueConstraints().add(constraint.get());
		return this;
	}
	
	@Override
	public CollectionImpl get() {
		return collection;
	}
}
