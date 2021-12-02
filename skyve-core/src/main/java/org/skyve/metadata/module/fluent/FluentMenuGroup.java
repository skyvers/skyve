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
	private Group group = null;
	
	public FluentMenuGroup() {
		group = new Group();
	}

	public FluentMenuGroup(Group group) {
		this.group = group;
	}

	public FluentMenuGroup from(@SuppressWarnings("hiding") MenuGroup group) {
		super.from(group);
		for (MenuItem item : group.getItems()) {
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
