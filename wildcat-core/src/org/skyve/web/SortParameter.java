package org.skyve.web;

import org.skyve.metadata.SortDirection;

/**
 * 
 */
public interface SortParameter {
	/**
	 * 
	 * @return
	 */
	public String getBinding();
	
	/**
	 * 
	 * @param binding
	 */
	public void setBinding(String binding);
	
	/**
	 * 
	 * @return
	 */
	public SortDirection getDirection();
	
	/**
	 * 
	 * @param direction
	 */
	public void setDirection(SortDirection direction);
}
