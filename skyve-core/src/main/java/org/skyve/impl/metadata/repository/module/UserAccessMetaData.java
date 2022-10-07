package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public abstract class UserAccessMetaData implements SerializableMetaData {
	private static final long serialVersionUID = -6274893418024546257L;

	private String moduleName;
	private List<UserAccessUxUiMetadata> uxuis = new ArrayList<>();

	public String getModuleName() {
		return moduleName;
	}

	@XmlAttribute(name = "module", required = true)
	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "uxui")
	public List<UserAccessUxUiMetadata> getUxuis() {
		return uxuis;
	}
}
