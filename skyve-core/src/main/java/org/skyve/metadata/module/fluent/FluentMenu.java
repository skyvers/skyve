package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.repository.module.Menu;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;

public class FluentMenu {
	private Menu menu = new Menu();
	
	public FluentMenu() {
		// nothing to see
	}
	
	public FluentMenu(org.skyve.metadata.module.menu.Menu menu) {
		for (MenuItem item :  menu.getItems()) {
			if (item instanceof ListItem) {
				addListItem(new FluentListItem((ListItem) item));
			}
			else if (item instanceof EditItem) {
				addEditItem(new FluentEditItem((EditItem) item));
			}
			else if (item instanceof MenuGroup) {
				addGroup(new FluentMenuGroup((MenuGroup) item));
			}
			else if (item instanceof MapItem) {
				addMapItem(new FluentMapItem((MapItem) item));
			}
			else if (item instanceof TreeItem) {
				addTreeItem(new FluentTreeItem((TreeItem) item));
			}
			else if (item instanceof CalendarItem) {
				addCalendarItem(new FluentCalendarItem((CalendarItem) item));
			}
			else if (item instanceof LinkItem) {
				addLinkItem(new FluentLinkItem((LinkItem) item));
			}
			else {
				throw new IllegalStateException(item + " not catered for");
			}
		}
	}
	
	public FluentMenu addGroup(FluentMenuGroup group) {
		menu.getActions().add(group.get());
		return this;
	}
	
	public FluentMenu addCalendarItem(FluentCalendarItem calendar) {
		menu.getActions().add(calendar.get());
		return this;
	}

	public FluentMenu addEditItem(FluentEditItem edit) {
		menu.getActions().add(edit.get());
		return this;
	}

	public FluentMenu addLinkItem(FluentLinkItem link) {
		menu.getActions().add(link.get());
		return this;
	}

	public FluentMenu addListItem(FluentListItem list) {
		menu.getActions().add(list.get());
		return this;
	}

	public FluentMenu addMapItem(FluentMapItem map) {
		menu.getActions().add(map.get());
		return this;
	}

	public FluentMenu addTreeItem(FluentTreeItem tree) {
		menu.getActions().add(tree.get());
		return this;
	}
	
	public Menu get() {
		return menu;
	}
}
