package org.skyve.metadata.model.document;

import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SortDirection;

import jakarta.xml.bind.annotation.XmlType;

/**
 * 
 */
public interface Collection extends Reference {
	/**
	 * 
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
	 * 
	 */
	public interface Ordering {
		/**
		 * 
		 * @return
		 */
		public String getBy();
		
		/**
		 * 
		 * @return
		 */
		public SortDirection getSort();
	}

	/**
	 * 
	 * @return
	 */
	public Boolean getOrdered();
	
	/**
	 * 
	 */
	@Override
	public CollectionType getType();
	
	/**
	 * This determines whether to create an index on the collection owner foreign key column.
	 * Set to true if the database does not implement foreign keys with indexes - ie SQLServer.
	 */
	public Boolean getOwnerDatabaseIndex();

	/**
	 * This determines whether to create an index on the collection element foreign key column.
	 * Set to true if the database does not implement foreign keys with indexes - ie SQLServer.
	 */
	public Boolean getElementDatabaseIndex();

	/**
	 * The name of the shared cache to use for this collection
	 */
	public String getCacheName();
	/**
	 * 
	 * @return
	 */
	public Integer getMaxCardinality();
	
	/**
	 * 
	 * @return
	 */
	public Integer getMinCardinality();
	
	/**
	 * 
	 * @return
	 */
	public List<Ordering> getOrdering();
	
	/**
	 * 
	 * @return
	 */
	public List<UniqueConstraint> getUniqueConstraints();
}
