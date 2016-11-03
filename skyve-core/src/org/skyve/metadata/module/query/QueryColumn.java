package org.skyve.metadata.module.query;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.SortDirection;

/**
 * 
 */
public interface QueryColumn extends NamedMetaData {
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
	public FilterOperator getFilterOperator();
	
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
	
	public Integer getPixelWidth();
	
	public HorizontalAlignment getAlignment();
}
