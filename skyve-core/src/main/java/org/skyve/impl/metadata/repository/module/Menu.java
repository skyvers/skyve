package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class Menu implements SerializableMetaData {
	private static final long serialVersionUID = 8381343095222755228L;

	private List<Action> actions = new ArrayList<>();

	@XmlElementRefs({@XmlElementRef(type = Item.class), @XmlElementRef(type = Group.class)})
	public List<Action> getActions() {
		return actions;
	}
}
