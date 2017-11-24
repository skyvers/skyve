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
	public String getBy();
	
	/**
	 * 
	 * @param by
	 */
	public void setBy(String by);
	
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
