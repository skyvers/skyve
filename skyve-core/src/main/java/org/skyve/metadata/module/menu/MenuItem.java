package org.skyve.metadata.module.menu;

import java.util.Set;

import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.util.Util;

/**
 * Represents a menu item. 
 */
public interface MenuItem extends NamedMetaData, DecoratedMetaData {
	public default String getLocalisedName() {
		return Util.i18n(getName());
	}
	
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
