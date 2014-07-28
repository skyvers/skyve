package org.skyve.metadata.module.query;

import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.SortDirection;
import org.skyve.wildcat.util.XMLUtil;

/**
 * 
 */
public interface QueryColumn extends NamedMetaData {
	/**
	 * 
	 */
	@XmlType(namespace = XMLUtil.MODULE_NAMESPACE)
	public enum OperatorType {
		/**
		 * 
		 */
		equal,
		
		/**
		 * 
		 */
		greater,
		
		/**
		 * 
		 */
		less,
		
		/**
		 * 
		 */
		greaterEqual,
		
		/**
		 * 
		 */
		lessEqual,
		
		/**
		 * 
		 */
		notEqual,
		
		/**
		 * 
		 */
		like, 
		
		/**
		 * 
		 */
		notLike, 
		
		/**
		 * 
		 */
		notNull,
		
		/**
		 * 
		 */
		isNull,
		
		/**
		 * 
		 */
		nullOrEqual,
		
		/**
		 * 
		 */
		nullOrGreater, 
		
		/**
		 * 
		 */
		nullOrLess,
		
		/**
		 * 
		 */
		nullOrGreaterEqual, 
		
		/**
		 * 
		 */
		nullOrLessEqual, 
		
		/**
		 * 
		 */
		nullOrNotEqual,
		
		/**
		 * 
		 */
		nullOrLike, 
		
		/**
		 * 
		 */
		nullOrNotLike;
	}

	/**
	 * 
	 * @return
	 */
	public String getDisplayName();
	
	/**
	 * 
	 * @return
	 */
	public String getBinding();
	
	/**
	 * 
	 * @return
	 */
	public String getExpression();
	
	/**
	 * 
	 * @return
	 */
	public OperatorType getFilterOperator();
	
	/**
	 * 
	 * @return
	 */
	public String getFilterExpression();
	
	/**
	 * 
	 * @return
	 */
	public SortDirection getSortOrder();
	
	/**
	 * 
	 * @return
	 */
	public boolean isProjected();

	/**
	 * 
	 * @return
	 */
	public boolean isHidden();
	
	/**
	 * 
	 * @return
	 */
	public boolean isFilterable();

	/**
	 * 
	 * @return
	 */
	public boolean isSortable();

	/**
	 * 
	 * @return
	 */
	public boolean isEditable();
}
