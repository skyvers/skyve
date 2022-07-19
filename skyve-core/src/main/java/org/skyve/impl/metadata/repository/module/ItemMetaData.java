package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "item")
public abstract class ItemMetaData extends ActionMetaData {
	private static final long serialVersionUID = -2533938765989721342L;

	private List<GrantedTo> roles = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "role", required = true)
	public List<GrantedTo> getRoles() {
		return roles;
	}
}
