package org.skyve.impl.metadata.repository;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public abstract class NamedMetaData implements org.skyve.metadata.NamedMetaData {
	private static final long serialVersionUID = 3158067742748907120L;

	private String name;

	@Override
	public String getName() {
		return name;
	}

	@XmlAttribute
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}
}
