package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class Menu {
	private List<Action> actions = new ArrayList<>();

	@XmlElementRefs({@XmlElementRef(type = Item.class), @XmlElementRef(type = Group.class)})
	public List<Action> getActions() {
		return actions;
	}
}
