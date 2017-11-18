package org.skyve.impl.metadata.module.menu;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.module.menu.MenuItem;

/**
 * Represents a menu.
 */
public class Menu implements org.skyve.metadata.module.menu.Menu {
	private static final long serialVersionUID = -8289343836260930093L;

	private List<MenuItem> menuItemList = new ArrayList<>();

	@Override
	public List<MenuItem> getItems() {
		return menuItemList;
	}

	/**
	 * Is the menu applicable to this uxui?
	 * Only if at least one of its items is applicable
	 */
	@Override
	public boolean isApplicable(String uxui) {
		boolean result = false;

		for (MenuItem item : menuItemList) {
			if (item.isApplicable(uxui)) {
				result = true;
				break;
			}
		}
		
		return result;
	}

	/**
	 * Provide a <code>String</code> representation of the menu.
	 * 
	 * @return The <code>String</code> representation.
	 */
	@Override
	public String toString() {
		StringBuilder menuStructure = new StringBuilder();

		for (MenuItem item : getItems()) {
			menuStructure.append("  ").append(item.toString());
		}

		return menuStructure.toString();
	}
}