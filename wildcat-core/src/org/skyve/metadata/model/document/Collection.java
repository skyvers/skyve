package org.skyve.metadata.model.document;

import java.util.List;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLUtil;
import org.skyve.metadata.SortDirection;

/**
 * 
 */
public interface Collection extends Reference {
	
	/**
	 * 
	 */
	@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
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
