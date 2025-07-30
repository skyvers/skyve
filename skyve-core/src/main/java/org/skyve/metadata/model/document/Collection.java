package org.skyve.metadata.model.document;

import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.Ordering;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlType;

/**
 * A list of related document instances.
 */
public interface Collection extends Reference {
	/**
	 * The type (database structure and behaviour) of a collection.
	 */
	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public enum CollectionType implements ReferenceType {
		/**
		 * Composition relationship to a child document - child points back to parent.
		 */
		child,

		/**
		 * Composition relationship with a joining table.
		 */
		composition,

		/**
		 * Aggregation relationship without a joining table.
		 */
		aggregation;
	}

	/**
	 * Indicates if this collection can be ordered/reordered.
	 * This adds the "bizOrdinal" implicit column to the data store to record an intentional ordering.
	 * @return	true to order, false or null is unordered
	 */
	public @Nullable Boolean getOrdered();
	
	/**
	 * 
	 */
	@Override
	public @Nonnull CollectionType getType();
	
	/**
	 * This determines whether to create an index on the collection owner foreign key column.
	 * Set to true if the database does not implement foreign keys with indexes - ie SQLServer.
	 */
	public @Nullable Boolean getOwnerDatabaseIndex();

	/**
	 * This determines whether to create an index on the collection element foreign key column.
	 * Set to true if the database does not implement foreign keys with indexes - ie SQLServer.
	 */
	public @Nullable Boolean getElementDatabaseIndex();

	/**
	 * The name of the shared cache to use for this collection
	 */
	public @Nullable String getCacheName();

	/**
	 * The minimum number of elements required in this collection to be valid.
	 * @return	The minimum cardinality required.
	 */
	public int getMinCardinality();

	/**
	 * The minimum number of elements allowed in this collection to be valid.
	 * @return	The maximum cardinality allowed.
	 */
	public @Nullable Integer getMaxCardinality();
	
	/**
	 * The collection ordering Skyve should endevour to keep.
	 * @return	The collection ordering.
	 */
	public @Nonnull List<Ordering> getOrdering();
	
	/**
	 * Unique constraints to be adhered to within this collection.
	 * @return	Any unique constraints.
	 */
	public @Nonnull List<UniqueConstraint> getUniqueConstraints();
}
