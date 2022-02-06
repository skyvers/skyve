package org.skyve.metadata.module.menu;

import java.util.List;

import org.skyve.metadata.SerializableMetaData;

/**
 * Represents a menu.
 */
public interface Menu extends SerializableMetaData {
	/**
	 * 
	 * @return
	 */
	public List<MenuItem> getItems();
	
	
	public boolean isApplicable(String uxui);
}