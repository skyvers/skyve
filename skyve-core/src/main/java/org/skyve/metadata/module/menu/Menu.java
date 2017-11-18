package org.skyve.metadata.module.menu;

import java.util.List;

import org.skyve.metadata.MetaData;

/**
 * Represents a menu.
 */
public interface Menu extends MetaData {
	/**
	 * 
	 * @return
	 */
	public List<MenuItem> getItems();
	
	
	public boolean isApplicable(String uxui);
}