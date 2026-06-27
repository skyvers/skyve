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
import org.skyve.impl.metadata.repository.module.TreeItemMetaData;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;

/**
 * Builds grouped menu metadata with nested actions and sub-groups.
 */
public class FluentMenuGroup extends FluentMenuAction<FluentMenuGroup> {
	private GroupMetaData group = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentMenuGroup() {
		group = new GroupMetaData();
	}

	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param group The metadata to mutate.
	 */
	public FluentMenuGroup(GroupMetaData group) {
		this.group = group;
	}

	/**
	 * Copies a menu group and all nested actions.
	 *
	 * <p>Side effects: appends converted action metadata to this wrapper's backing group.
	 *
	 * @param group The source menu group.
	 * @return this fluent instance.
	 * @throws IllegalStateException If a source menu item type is not supported.
	 */
	public FluentMenuGroup from(@SuppressWarnings("hiding") MenuGroup group) {
		super.from(group);
		for (MenuItem item : group.getItems()) {
			if (item instanceof EditItem edit) {
				addEditItem(new FluentEditItem().from(edit));
			}
			else if (item instanceof TreeItem tree) {
				addTreeItem(new FluentTreeItem().from(tree));
			}
			else if (item instanceof ListItem list) {
				addListItem(new FluentListItem().from(list));
			}
			else if (item instanceof MenuGroup thisGroup) {
				addGroup(new FluentMenuGroup().from(thisGroup));
			}
			else if (item instanceof MapItem map) {
				addMapItem(new FluentMapItem().from(map));
			}
			else if (item instanceof CalendarItem calendar) {
				addCalendarItem(new FluentCalendarItem().from(calendar));
			}
			else if (item instanceof LinkItem link) {
				addLinkItem(new FluentLinkItem().from(link));
			}
			else {
				throw new IllegalStateException(item + " not catered for");
			}
		}
		return this;
	}
	
	private ActionMetaData findAction(String name) {
		return group.getActions().stream().filter(a -> name.equals(a.getName())).findAny().orElse(null);
	}
	
	/**
	 * Adds a nested menu group action.
	 *
	 * @param subGroup The nested group wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenuGroup addGroup(FluentMenuGroup subGroup) {
		group.getActions().add(subGroup.get());
		return this;
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
	 * Adds a calendar menu item action.
	 *
	 * @param calendar The calendar item wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenuGroup addCalendarItem(FluentCalendarItem calendar) {
		group.getActions().add(calendar.get());
		return this;
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
	 * Adds an edit menu item action.
	 *
	 * @param edit The edit item wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenuGroup addEditItem(FluentEditItem edit) {
		group.getActions().add(edit.get());
		return this;
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
	 * Adds an external link menu item action.
	 *
	 * @param link The link item wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenuGroup addLinkItem(FluentLinkItem link) {
		group.getActions().add(link.get());
		return this;
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
	 * Adds a list menu item action.
	 *
	 * @param list The list item wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenuGroup addListItem(FluentListItem list) {
		group.getActions().add(list.get());
		return this;
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
	 * Adds a map menu item action.
	 *
	 * @param map The map item wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenuGroup addMapItem(FluentMapItem map) {
		group.getActions().add(map.get());
		return this;
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
	 * Adds a tree menu item action.
	 *
	 * @param tree The tree item wrapper.
	 * @return this fluent instance.
	 */
	public FluentMenuGroup addTreeItem(FluentTreeItem tree) {
		group.getActions().add(tree.get());
		return this;
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
	 * Removes group actions matching the supplied action name.
	 *
	 * @param name The action name.
	 * @return this fluent instance.
	 */
	public FluentMenuGroup removeMenuAction(String name) {
		group.getActions().removeIf(a -> name.equals(a.getName()));
		return this;
	}

	/**
	 * Removes all actions configured on this group.
	 *
	 * @return this fluent instance.
	 */
	public FluentMenuGroup clearMenuActions() {
		group.getActions().clear();
		return this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The group metadata instance.
	 */
	@Override
	public GroupMetaData get() {
		return group;
	}
}
