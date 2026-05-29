package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.OrderingImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.UniqueConstraintImpl;
import org.skyve.metadata.Ordering;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.UniqueConstraint;

/**
 * Builds {@link CollectionImpl} metadata using fluent mutation methods.
 */
public class FluentCollection extends FluentReference<FluentCollection> {
	private CollectionImpl collection = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentCollection() {
		collection = new CollectionImpl();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentCollection(CollectionImpl collection) {
		this.collection = collection;
	}

	/**
	 * Copies collection metadata, including ordering and unique-constraint definitions.
	 */
	public FluentCollection from(@SuppressWarnings("hiding") Collection collection) {
		super.from(collection);
		type(collection.getType());
		ordered(Boolean.TRUE.equals(collection.getOrdered()));
		minCardinality(collection.getMinCardinality());
		Integer i = collection.getMaxCardinality();
		if (i != null) {
			maxCardinality(i.intValue());
		}
		Boolean index = collection.getOwnerDatabaseIndex();
		if (index != null) {
			ownerDatabaseIndex(index.booleanValue());
		}
		index = collection.getElementDatabaseIndex();
		if (index != null) {
			elementDatabaseIndex(index.booleanValue());
		}
		cacheName(collection.getCacheName());

		for (Ordering ordering : collection.getOrdering()) {
			addOrdering(new FluentCollectionOrdering().from(ordering));
		}
		
		for (UniqueConstraint constraint : collection.getUniqueConstraints()) {
			addUniqueConstraint(new FluentCollectionUniqueConstraint().from(constraint));
		}
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCollection type(CollectionType type) {
		collection.setType(type);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCollection ordered(boolean ordered) {
		collection.setOrdered(ordered ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCollection minCardinality(int minCardinality) {
		collection.setMinCardinality(minCardinality);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCollection maxCardinality(int maxCardinality) {
		collection.setMaxCardinality(Integer.valueOf(maxCardinality));
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCollection ownerDatabaseIndex(boolean ownerDatabaseIndex) {
		collection.setOwnerDatabaseIndex(ownerDatabaseIndex ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCollection elementDatabaseIndex(boolean elementDatabaseIndex) {
		collection.setElementDatabaseIndex(elementDatabaseIndex ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentCollection cacheName(String cacheName) {
		collection.setCacheName(cacheName);
		return this;
	}
	
	/**
	 * Appends an ordering definition.
	 *
	 * <p>Side effects: mutates the ordering list in insertion order.
	 */
	public FluentCollection addOrdering(FluentCollectionOrdering ordering) {
		collection.getOrdering().add(ordering.get());
		return this;
	}

	/**
	 * Removes ordering definitions that match {@code by}.
	 */
	public FluentCollection removeOrdering(String by) {
		collection.getOrdering().removeIf(o -> by.equals(o.getBy()));
		return this;
	}
	/**
	 * Clears all matching metadata definitions from this builder.
	 */
	public FluentCollection clearOrdering() {
		collection.getOrdering().clear();
		return this;
	}

	/**
	 * Finds the first ordering definition that matches {@code by}.
	 *
	 * @return a fluent wrapper around the matching ordering, or {@code null} if none match
	 */
	public FluentCollectionOrdering findOrdering(String by) {
		OrderingImpl result = (OrderingImpl) collection.getOrdering().stream().filter(o -> by.equals(o.getBy())).findAny().orElse(null);
		if (result != null) {
			return new FluentCollectionOrdering(result);
		}
		return null;
	}

	/**
	 * Appends a unique-constraint definition.
	 */
	public FluentCollection addUniqueConstraint(FluentCollectionUniqueConstraint constraint) {
		collection.getUniqueConstraints().add(constraint.get());
		return this;
	}
	/**
	 * Removes matching metadata definitions from this builder.
	 */
	public FluentCollection removeUniqueConstraint(String name) {
		collection.getUniqueConstraints().removeIf(u -> name.equals(u.getName()));
		return this;
	}
	/**
	 * Clears all matching metadata definitions from this builder.
	 */
	public FluentCollection clearUniqueConstraints() {
		collection.getUniqueConstraints().clear();
		return this;
	}
	
	/**
	 * Finds the first unique-constraint definition that matches {@code name}.
	 *
	 * @return a fluent wrapper around the matching constraint, or {@code null} if none match
	 */
	public FluentCollectionUniqueConstraint findUniqueConstraint(String name) {
		UniqueConstraintImpl result = (UniqueConstraintImpl) collection.getUniqueConstraints().stream().filter(u -> name.equals(u.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCollectionUniqueConstraint(result);
		}
		return null;
	}

	/**
	 * Returns the mutable collection metadata instance being built.
	 */
	@Override
	public CollectionImpl get() {
		return collection;
	}
}
