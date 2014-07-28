package org.skyve.wildcat.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.MODULE_NAMESPACE)
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
