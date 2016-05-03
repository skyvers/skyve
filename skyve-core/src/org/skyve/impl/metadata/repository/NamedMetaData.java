package org.skyve.impl.metadata.repository;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLUtil;

@XmlType(namespace = XMLUtil.COMMON_NAMESPACE)
public abstract class NamedMetaData {
	private String name;

	public String getName() {
		return name;
	}

	@XmlAttribute
	public void setName(String name) {
		this.name = name;
	}
}
