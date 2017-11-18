package org.skyve.impl.metadata.module.menu;

import java.util.Set;
import java.util.TreeSet;

import org.skyve.metadata.module.menu.MenuItem;

/**
 * Represents a menu item leaf node that can be used for navigation. 
 * This class comprises the Leaf part of the GoF composite pattern.
 */
public abstract class AbstractMenuItem implements MenuItem {
	private static final long serialVersionUID = -5028812690596668599L;

	/**
	 * The name of the menu item.
	 */
	private String name;

	private Set<String> roleNames = new TreeSet<>();

	private Set<String> uxuis = new TreeSet<>();
	
	/**
	 * From {@link MenuItem}.
	 */
	@Override
	public final String getName() {
		return name;
	}

	/**
	 * From {@link MenuItem}.
	 */
	public final void setName(String name) {
		this.name = name;
	}

	@Override
	public final Set<String> getRoleNames() {
		return roleNames;
	}

	@Override
	public final Set<String> getUxUis() {
		return uxuis;
	}

	@Override
	public boolean isApplicable(String uxui) {
		return uxuis.isEmpty() || uxuis.contains(uxui);
	}
}
