package org.skyve.metadata.module.query;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.SortDirection;
import org.skyve.util.Util;

/**
 * 
 */
public interface MetaDataQueryColumn extends NamedMetaData {
	/**
	 * 
	 * @return
	 */
	public String getDisplayName();
	
	public default String getLocalisedDisplayName() {
		return Util.i18n(getDisplayName());
	}
	
	/**
	 * 
	 * @return
	 */
	public String getBinding();
	
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
	public boolean isHidden();
	
	public Integer getPixelWidth();
	
	public HorizontalAlignment getAlignment();
}
