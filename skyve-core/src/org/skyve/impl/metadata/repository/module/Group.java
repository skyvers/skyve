package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLUtil;

@XmlType(namespace = XMLUtil.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.MODULE_NAMESPACE)
public class Group extends Action {
	private List<Action> actions = new ArrayList<>();

	@XmlElementRefs({@XmlElementRef(type = EditItem.class),
						@XmlElementRef(type = ListItem.class),
						@XmlElementRef(type = MapItem.class),
						@XmlElementRef(type = CalendarItem.class),
						@XmlElementRef(type = TreeItem.class),
						@XmlElementRef(type = LinkItem.class),
						@XmlElementRef(type = Group.class)})
	public List<Action> getActions() {
		return actions;
	}
}
