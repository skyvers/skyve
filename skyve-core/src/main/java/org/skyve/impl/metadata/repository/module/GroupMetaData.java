package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <group>} menu separator/sub-menu in a
 * module menu.
 *
 * <p>A group is a named menu container that holds an ordered list of child menu
 * items.  Extends {@link ActionMetaData} to carry the group name.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see MenuMetaData
 * @see ItemMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "group")
public class GroupMetaData extends ActionMetaData {
	private static final long serialVersionUID = -8705078693132357242L;

	private List<ActionMetaData> actions = new ArrayList<>();

	@XmlElementRefs({@XmlElementRef(type = EditItemMetaData.class),
						@XmlElementRef(type = ListItemMetaData.class),
						@XmlElementRef(type = MapItemMetaData.class),
						@XmlElementRef(type = CalendarItemMetaData.class),
						@XmlElementRef(type = TreeItemMetaData.class),
						@XmlElementRef(type = LinkItemMetaData.class),
						@XmlElementRef(type = GroupMetaData.class)})
	public List<ActionMetaData> getActions() {
		return actions;
	}
}
