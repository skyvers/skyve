package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class GrantedTo {
	private String roleName;

	public String getRoleName() {
		return roleName;
	}

	@XmlAttribute(name = "name", required = true)
	public void setRoleName(String roleName) {
		this.roleName = UtilImpl.processStringValue(roleName);
	}
}
