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

public class FluentMenu {
	private MenuMetaData menu = null;
	
	public FluentMenu() {
		menu = new MenuMetaData();
	}

	public FluentMenu(MenuMetaData menu) {
		this.menu = menu;
	}

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
	
	public FluentMenu addAction(FluentMenuAction<?> action) {
		menu.getActions().add(action.get());
		return this;
	}
	
	public FluentMenu addGroup(FluentMenuGroup group) {
		return addAction(group);
	}

	public FluentMenuGroup findGroup(String name) {
		GroupMetaData result = (GroupMetaData) findAction(name);
		if (result != null) {
			return new FluentMenuGroup(result);
		}
		return null;
	}
	
	public FluentMenu addCalendarItem(FluentCalendarItem calendar) {
		return addAction(calendar);
	}

	public FluentCalendarItem findCalendarItem(String name) {
		CalendarItemMetaData result = (CalendarItemMetaData) findAction(name);
		if (result != null) {
			return new FluentCalendarItem(result);
		}
		return null;
	}
	
	public FluentMenu addEditItem(FluentEditItem edit) {
		return addAction(edit);
	}

	public FluentEditItem findEditItem(String name) {
		EditItemMetaData result = (EditItemMetaData) findAction(name);
		if (result != null) {
			return new FluentEditItem(result);
		}
		return null;
	}
	
	public FluentMenu addLinkItem(FluentLinkItem link) {
		return addAction(link);
	}

	public FluentLinkItem findLinkItem(String name) {
		LinkItemMetaData result = (LinkItemMetaData) findAction(name);
		if (result != null) {
			return new FluentLinkItem(result);
		}
		return null;
	}
	
	public FluentMenu addListItem(FluentListItem list) {
		return addAction(list);
	}

	public FluentListItem findListItem(String name) {
		ListItemMetaData result = (ListItemMetaData) findAction(name);
		if (result != null) {
			return new FluentListItem(result);
		}
		return null;
	}
	
	public FluentMenu addMapItem(FluentMapItem map) {
		return addAction(map);
	}

	public FluentMapItem findMapItem(String name) {
		MapItemMetaData result = (MapItemMetaData) findAction(name);
		if (result != null) {
			return new FluentMapItem(result);
		}
		return null;
	}
	
	public FluentMenu addTreeItem(FluentTreeItem tree) {
		return addAction(tree);
	}
	
	public FluentTreeItem findTreeItem(String name) {
		TreeItemMetaData result = (TreeItemMetaData) findAction(name);
		if (result != null) {
			return new FluentTreeItem(result);
		}
		return null;
	}
	
	public FluentMenu removeMenuAction(String name) {
		menu.getActions().removeIf(a -> name.equals(a.getName()));
		return this;
	}	

	public FluentMenu clearMenuActions() {
		menu.getActions().clear();
		return this;
	}	

	public MenuMetaData get() {
		return menu;
	}
}
