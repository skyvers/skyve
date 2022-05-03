package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.repository.module.Action;
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
	
	private Action findAction(String name) {
		return group.getActions().stream().filter(a -> name.equals(a.getName())).findAny().orElse(null);
	}
	
	public FluentMenuGroup addGroup(FluentMenuGroup subGroup) {
		group.getActions().add(subGroup.get());
		return this;
	}
	
	public FluentMenuGroup findGroup(String name) {
		Group result = (Group) findAction(name);
		if (result != null) {
			return new FluentMenuGroup(result);
		}
		return null;
	}
	
	public FluentMenuGroup addCalendarItem(FluentCalendarItem calendar) {
		group.getActions().add(calendar.get());
		return this;
	}

	public FluentCalendarItem findCalendarItem(String name) {
		org.skyve.impl.metadata.repository.module.CalendarItem result = (org.skyve.impl.metadata.repository.module.CalendarItem) findAction(name);
		if (result != null) {
			return new FluentCalendarItem(result);
		}
		return null;
	}
	
	public FluentMenuGroup addEditItem(FluentEditItem edit) {
		group.getActions().add(edit.get());
		return this;
	}

	public FluentEditItem findEditItem(String name) {
		org.skyve.impl.metadata.repository.module.EditItem result = (org.skyve.impl.metadata.repository.module.EditItem) findAction(name);
		if (result != null) {
			return new FluentEditItem(result);
		}
		return null;
	}
	
	public FluentMenuGroup addLinkItem(FluentLinkItem link) {
		group.getActions().add(link.get());
		return this;
	}

	public FluentLinkItem findLinkItem(String name) {
		org.skyve.impl.metadata.repository.module.LinkItem result = (org.skyve.impl.metadata.repository.module.LinkItem) findAction(name);
		if (result != null) {
			return new FluentLinkItem(result);
		}
		return null;
	}
	
	public FluentMenuGroup addListItem(FluentListItem list) {
		group.getActions().add(list.get());
		return this;
	}

	public FluentListItem findListItem(String name) {
		org.skyve.impl.metadata.repository.module.ListItem result = (org.skyve.impl.metadata.repository.module.ListItem) findAction(name);
		if (result != null) {
			return new FluentListItem(result);
		}
		return null;
	}
	
	public FluentMenuGroup addMapItem(FluentMapItem map) {
		group.getActions().add(map.get());
		return this;
	}

	public FluentMapItem findMapItem(String name) {
		org.skyve.impl.metadata.repository.module.MapItem result = (org.skyve.impl.metadata.repository.module.MapItem) findAction(name);
		if (result != null) {
			return new FluentMapItem(result);
		}
		return null;
	}
	
	public FluentMenuGroup addTreeItem(FluentTreeItem tree) {
		group.getActions().add(tree.get());
		return this;
	}

	public FluentTreeItem findTreeItem(String name) {
		org.skyve.impl.metadata.repository.module.TreeItem result = (org.skyve.impl.metadata.repository.module.TreeItem) findAction(name);
		if (result != null) {
			return new FluentTreeItem(result);
		}
		return null;
	}
	
	public FluentMenuGroup removeMenuAction(String name) {
		group.getActions().removeIf(a -> name.equals(a.getName()));
		return this;
	}

	public FluentMenuGroup clearMenuActions() {
		group.getActions().clear();
		return this;
	}

	@Override
	public Group get() {
		return group;
	}
}
