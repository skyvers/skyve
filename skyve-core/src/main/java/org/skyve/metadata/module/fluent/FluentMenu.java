package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.repository.module.Action;
import org.skyve.impl.metadata.repository.module.Group;
import org.skyve.impl.metadata.repository.module.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;

public class FluentMenu {
	private Menu menu = null;
	
	public FluentMenu() {
		menu = new Menu();
	}

	public FluentMenu(Menu menu) {
		this.menu = menu;
	}

	public FluentMenu from(@SuppressWarnings("hiding") org.skyve.metadata.module.menu.Menu menu) {
		for (MenuItem item :  menu.getItems()) {
			if (item instanceof EditItem) {
				addEditItem(new FluentEditItem().from((EditItem) item));
			}
			else if (item instanceof TreeItem) {
				addTreeItem(new FluentTreeItem().from((TreeItem) item));
			}
			else if (item instanceof ListItem) {
				addListItem(new FluentListItem().from((ListItem) item));
			}
			else if (item instanceof MenuGroup) {
				addGroup(new FluentMenuGroup().from((MenuGroup) item));
			}
			else if (item instanceof MapItem) {
				addMapItem(new FluentMapItem().from((MapItem) item));
			}
			else if (item instanceof CalendarItem) {
				addCalendarItem(new FluentCalendarItem().from((CalendarItem) item));
			}
			else if (item instanceof LinkItem) {
				addLinkItem(new FluentLinkItem().from((LinkItem) item));
			}
			else {
				throw new IllegalStateException(item + " not catered for");
			}
		}
		return this;
	}
	
	private Action findAction(String name) {
		return menu.getActions().stream().filter(a -> name.equals(a.getName())).findAny().orElse(null);
	}
	
	public FluentMenu addGroup(FluentMenuGroup group) {
		menu.getActions().add(group.get());
		return this;
	}

	public FluentMenuGroup findGroup(String name) {
		Group result = (Group) findAction(name);
		if (result != null) {
			return new FluentMenuGroup(result);
		}
		return null;
	}
	
	public FluentMenu addCalendarItem(FluentCalendarItem calendar) {
		menu.getActions().add(calendar.get());
		return this;
	}

	public FluentCalendarItem findCalendarItem(String name) {
		org.skyve.impl.metadata.repository.module.CalendarItem result = (org.skyve.impl.metadata.repository.module.CalendarItem) findAction(name);
		if (result != null) {
			return new FluentCalendarItem(result);
		}
		return null;
	}
	
	public FluentMenu addEditItem(FluentEditItem edit) {
		menu.getActions().add(edit.get());
		return this;
	}

	public FluentEditItem findEditItem(String name) {
		org.skyve.impl.metadata.repository.module.EditItem result = (org.skyve.impl.metadata.repository.module.EditItem) findAction(name);
		if (result != null) {
			return new FluentEditItem(result);
		}
		return null;
	}
	
	public FluentMenu addLinkItem(FluentLinkItem link) {
		menu.getActions().add(link.get());
		return this;
	}

	public FluentLinkItem findLinkItem(String name) {
		org.skyve.impl.metadata.repository.module.LinkItem result = (org.skyve.impl.metadata.repository.module.LinkItem) findAction(name);
		if (result != null) {
			return new FluentLinkItem(result);
		}
		return null;
	}
	
	public FluentMenu addListItem(FluentListItem list) {
		menu.getActions().add(list.get());
		return this;
	}

	public FluentListItem findListItem(String name) {
		org.skyve.impl.metadata.repository.module.ListItem result = (org.skyve.impl.metadata.repository.module.ListItem) findAction(name);
		if (result != null) {
			return new FluentListItem(result);
		}
		return null;
	}
	
	public FluentMenu addMapItem(FluentMapItem map) {
		menu.getActions().add(map.get());
		return this;
	}

	public FluentMapItem findMapItem(String name) {
		org.skyve.impl.metadata.repository.module.MapItem result = (org.skyve.impl.metadata.repository.module.MapItem) findAction(name);
		if (result != null) {
			return new FluentMapItem(result);
		}
		return null;
	}
	
	public FluentMenu addTreeItem(FluentTreeItem tree) {
		menu.getActions().add(tree.get());
		return this;
	}
	
	public FluentTreeItem findTreeItem(String name) {
		org.skyve.impl.metadata.repository.module.TreeItem result = (org.skyve.impl.metadata.repository.module.TreeItem) findAction(name);
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

	public Menu get() {
		return menu;
	}
}
