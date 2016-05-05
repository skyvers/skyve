package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE)
public abstract class Item extends Action {
	private List<GrantedTo> roles = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "role", required = true)
	public List<GrantedTo> getRoles() {
		return roles;
	}
}
