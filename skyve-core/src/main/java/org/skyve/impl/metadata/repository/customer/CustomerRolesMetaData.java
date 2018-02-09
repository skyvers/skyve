package org.skyve.impl.metadata.repository.customer;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "roles")
public class CustomerRolesMetaData {
	private List<CustomerRoleMetaData> roles = new ArrayList<>();
	private boolean allowModuleRoles = true;
	
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "role", required = true)
	public List<CustomerRoleMetaData> getRoles() {
		return roles;
	}
	
	public boolean isAllowModuleRoles() {
		return allowModuleRoles;
	}

	@XmlAttribute(required = true)
	public void setAllowModuleRoles(boolean allowModuleRoles) {
		this.allowModuleRoles = allowModuleRoles;
	}
}
