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
