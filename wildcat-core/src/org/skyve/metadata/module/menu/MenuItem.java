package org.skyve.metadata.module.menu;

import java.util.Set;

import org.skyve.metadata.NamedMetaData;

/**
 * Represents a menu. 
 * This class comprises the Component part of the GoF composite pattern.
 */
public interface MenuItem extends NamedMetaData {
	/**
	 * Get the list of Role names allowed access to this menu item.
	 */
	public Set<String> getRoleNames();
	
	/**
	 * Get the list of UX/UIs allowed access to this menu item.
	 * An empty list means all can access.
	 */
	public Set<String> getUxUis();
	
	public boolean isApplicable(String uxui);
}
