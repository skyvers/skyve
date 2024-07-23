package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class GrantedTo implements SerializableMetaData {
	private static final long serialVersionUID = -7663187695188357387L;

	private String roleName;

	public String getRoleName() {
		return roleName;
	}

	@XmlAttribute(name = "name", required = true)
	public void setRoleName(String roleName) {
		this.roleName = UtilImpl.processStringValue(roleName);
	}
}
