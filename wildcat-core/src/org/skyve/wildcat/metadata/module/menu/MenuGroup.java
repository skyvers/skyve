package org.skyve.wildcat.metadata.module.menu;

import java.util.Set;
import java.util.TreeSet;

import org.skyve.metadata.module.menu.MenuItem;

/**
 * Represents a menu group (composite of {@link MenuItem}s). 
 * This class comprises the Composite part of the GoF composite pattern.
 */
public final class MenuGroup extends Menu implements org.skyve.metadata.module.menu.MenuGroup {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 7566033386058173741L;

	/**
	 * The name of this menu group.
	 */
	private String name;

	private Set<String> uxuis = new TreeSet<>();

	/**
	 * Javadoc from revsa.revnet.menu.MenuItem
	 */
	@Override
	public final String getName() {
		return name;
	}

	/**
	 * Javadoc from revsa.revnet.menu.MenuItem
	 */
	public final void setName(String name) {
		this.name = name;
	} // setName

	/**
	 * From {@link Menu}.
	 */
	@Override
	public final String toString() {
		StringBuilder menuStructure = new StringBuilder();

		if (getName() != null) {
			menuStructure.append(getName());
		}

		for (MenuItem item : getItems()) {
			menuStructure.append("  ").append(item.toString());
		}

		return menuStructure.toString();
	}

	@Override
	public final Set<String> getRoleNames() {
		return null;
	}

	@Override
	public final Set<String> getUxUis() {
		return uxuis;
	}

	/**
	 * Only applicable if there are no uxuis defined and it has at least 1 item that is applicable OR
	 * there are uxuis defined and it contains this uxui.
	 */
	@Override
	public boolean isApplicable(String uxui) {
		boolean result = false;
		if (uxuis.isEmpty()) {
			for (MenuItem item : getItems()) {
				if (item.isApplicable(uxui)) {
					result = true;
					break;
				}
			}
		}
		else {
			result = uxuis.contains(uxui);
		}
		
		return result;
	}
}
