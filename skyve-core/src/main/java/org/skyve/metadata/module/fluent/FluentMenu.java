package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.repository.module.ActionMetaData;
import org.skyve.impl.metadata.repository.module.CalendarItemMetaData;
import org.skyve.impl.metadata.repository.module.EditItemMetaData;
import org.skyve.impl.metadata.repository.module.GroupMetaData;
import org.skyve.impl.metadata.repository.module.LinkItemMetaData;
import org.skyve.impl.metadata.repository.module.ListItemMetaData;
import org.skyve.impl.metadata.repository.module.MapItemMetaData;
import org.skyve.impl.metadata.repository.module.MenuMetaData;
import org.skyve.impl.metadata.repository.module.TreeItemMetaData;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;

/**
 * Builds module menu metadata, including nested groups and concrete item types.
 */
public class FluentMenu {
	private MenuMetaData menu = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentMenu() {
		menu = new MenuMetaData();
	}

	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param menu The metadata to mutate.
	 */
	public FluentMenu(MenuMetaData menu) {
		this.menu = menu;
	}

	/**
	 * Copies all menu actions from an existing menu.
	 *
	 * <p>Side effects: appends converted action metadata to this wrapper's backing menu.
	 *
	 * @param menu The source menu.
	 * @return this fluent instance.
	 * @throws IllegalStateException If a source menu item type is not supported.
	 */
	public FluentMenu from(@SuppressWarnings("hiding") org.skyve.metadata.module.menu.Menu menu) {
		for (MenuItem item :  menu.getItems()) {
			if (item instanceof EditItem edit) {
				addAction(new FluentEditItem().from(edit));
			}
			else if (item instanceof TreeItem tree) {
				addAction(new FluentTreeItem().from(tree));
			}
			else if (item instanceof ListItem list) {
				addAction(new FluentListItem().from(list));
			}
			else if (item instanceof MenuGroup group) {
				addAction(new FluentMenuGroup().from(group));
			}
			else if (item instanceof MapItem map) {
				addAction(new FluentMapItem().from(map));
			}
			else if (item instanceof CalendarItem calendar) {
				addAction(new FluentCalendarItem().from(calendar));
			}
			else if (item instanceof LinkItem link) {
				addAction(new FluentLinkItem().from(link));
			}
			else {
				throw new IllegalStateException(item + " not catered for");
			}
		}
		return this;
	}
	
	private ActionMetaData findAction(String name) {
		return menu.getActions().stream().filter(a -> name.equals(a.getName())).findAny().orElse(null);
	}
	
	/**
	 * Adds a menu action entry.
	 *
	 * @param action The menu action wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenu addAction(FluentMenuAction<?> action) {
		menu.getActions().add(action.get());
		return this;
	}
	
	/**
	 * Adds a menu group action entry.
	 *
	 * @param group The group wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenu addGroup(FluentMenuGroup group) {
		return addAction(group);
	}

	/**
	 * Finds and returns the matching item from the wrapped metadata, or null when none matches.
	 */
	public FluentMenuGroup findGroup(String name) {
		GroupMetaData result = (GroupMetaData) findAction(name);
		if (result != null) {
			return new FluentMenuGroup(result);
		}
		return null;
	}
	
	/**
	 * Adds a calendar menu item entry.
	 *
	 * @param calendar The calendar item wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenu addCalendarItem(FluentCalendarItem calendar) {
		return addAction(calendar);
	}

	/**
	 * Finds and returns the matching item from the wrapped metadata, or null when none matches.
	 */
	public FluentCalendarItem findCalendarItem(String name) {
		CalendarItemMetaData result = (CalendarItemMetaData) findAction(name);
		if (result != null) {
			return new FluentCalendarItem(result);
		}
		return null;
	}
	
	/**
	 * Adds an edit menu item entry.
	 *
	 * @param edit The edit item wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenu addEditItem(FluentEditItem edit) {
		return addAction(edit);
	}

	/**
	 * Finds and returns the matching item from the wrapped metadata, or null when none matches.
	 */
	public FluentEditItem findEditItem(String name) {
		EditItemMetaData result = (EditItemMetaData) findAction(name);
		if (result != null) {
			return new FluentEditItem(result);
		}
		return null;
	}
	
	/**
	 * Adds an external link menu item entry.
	 *
	 * @param link The link item wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenu addLinkItem(FluentLinkItem link) {
		return addAction(link);
	}

	/**
	 * Finds and returns the matching item from the wrapped metadata, or null when none matches.
	 */
	public FluentLinkItem findLinkItem(String name) {
		LinkItemMetaData result = (LinkItemMetaData) findAction(name);
		if (result != null) {
			return new FluentLinkItem(result);
		}
		return null;
	}
	
	/**
	 * Adds a list menu item entry.
	 *
	 * @param list The list item wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenu addListItem(FluentListItem list) {
		return addAction(list);
	}

	/**
	 * Finds and returns the matching item from the wrapped metadata, or null when none matches.
	 */
	public FluentListItem findListItem(String name) {
		ListItemMetaData result = (ListItemMetaData) findAction(name);
		if (result != null) {
			return new FluentListItem(result);
		}
		return null;
	}
	
	/**
	 * Adds a map menu item entry.
	 *
	 * @param map The map item wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenu addMapItem(FluentMapItem map) {
		return addAction(map);
	}

	/**
	 * Finds and returns the matching item from the wrapped metadata, or null when none matches.
	 */
	public FluentMapItem findMapItem(String name) {
		MapItemMetaData result = (MapItemMetaData) findAction(name);
		if (result != null) {
			return new FluentMapItem(result);
		}
		return null;
	}
	
	/**
	 * Adds a tree menu item entry.
	 *
	 * @param tree The tree item wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenu addTreeItem(FluentTreeItem tree) {
		return addAction(tree);
	}
	
	/**
	 * Finds and returns the matching item from the wrapped metadata, or null when none matches.
	 */
	public FluentTreeItem findTreeItem(String name) {
		TreeItemMetaData result = (TreeItemMetaData) findAction(name);
		if (result != null) {
			return new FluentTreeItem(result);
		}
		return null;
	}
	
	/**
	 * Removes menu actions matching the supplied action name.
	 *
	 * @param name The action name.
	 * @return this fluent instance.
	 */
	public FluentMenu removeMenuAction(String name) {
		menu.getActions().removeIf(a -> name.equals(a.getName()));
		return this;
	}	

	/**
	 * Removes all configured menu actions.
	 *
	 * @return this fluent instance.
	 */
	public FluentMenu clearMenuActions() {
		menu.getActions().clear();
		return this;
	}	

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The menu metadata instance.
	 */
	public MenuMetaData get() {
		return menu;
	}
}
