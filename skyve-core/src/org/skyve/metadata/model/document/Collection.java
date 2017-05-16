package org.skyve.metadata.model.document;

import java.util.List;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SortDirection;

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
	 * Cascade type 'merge' makes many-many relationships within the association
	 * target object update (without the collection being dirty)
	 * and thus causes optimistic lock exceptions when the bizLock 
	 * is up-revved from the update statement.
	 * Case in point is Staff --many-to-one--> User --many-to-many--> Groups,
	 * all groups are up-revved, even though the collection is not dirty,
	 * causing optimistic lock when Staff are saved.
	 * So if lots of Staff use the same user, we're screwed.
	 */
	public Boolean getAllowCascadeMerge();

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
