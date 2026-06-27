package org.skyve.impl.metadata.repository.customer;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for the {@code <roles>} element in a
 * {@code customer.xml} file.
 *
 * <p>Holds the ordered list of customer-defined composite roles and the
 * {@code allowModuleRoles} flag which controls whether individual module roles
 * can be assigned directly to users.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see CustomerRoleMetaData
 */
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
