package org.skyve.metadata.module.menu;

import java.util.List;

import org.skyve.metadata.DecoratedMetaData;

/**
 * Represents a menu.
 */
public interface Menu extends DecoratedMetaData {
	/**
	 * 
	 * @return
	 */
	public List<MenuItem> getItems();
	
	
	public boolean isApplicable(String uxui);
}
