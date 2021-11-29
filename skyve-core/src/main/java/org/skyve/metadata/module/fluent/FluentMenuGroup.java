package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.repository.module.Group;
import org.skyve.metadata.module.menu.MenuGroup;
import org.skyve.metadata.module.menu.MenuItem;

public class FluentMenuGroup extends FluentMenuAction<FluentMenuGroup> {
	private Group group = new Group();
	
	public FluentMenuGroup() {
		// nothing to see
	}

	public FluentMenuGroup(MenuGroup group) {
		super(group);
		for (MenuItem item : group.getItems()) {
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
	
	public FluentMenuGroup addGroup(FluentMenuGroup subGroup) {
		group.getActions().add(subGroup.get());
		return this;
	}
	
	public FluentMenuGroup addCalendarItem(FluentCalendarItem calendar) {
		group.getActions().add(calendar.get());
		return this;
	}

	public FluentMenuGroup addEditItem(FluentEditItem edit) {
		group.getActions().add(edit.get());
		return this;
	}

	public FluentMenuGroup addLinkItem(FluentLinkItem link) {
		group.getActions().add(link.get());
		return this;
	}

	public FluentMenuGroup addListItem(FluentListItem list) {
		group.getActions().add(list.get());
		return this;
	}

	public FluentMenuGroup addMapItem(FluentMapItem map) {
		group.getActions().add(map.get());
		return this;
	}

	public FluentMenuGroup addTreeItem(FluentTreeItem tree) {
		group.getActions().add(tree.get());
		return this;
	}

	@Override
	public Group get() {
		return group;
	}
}
