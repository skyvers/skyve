package org.skyve.impl.metadata.repository.customer;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.CustomerRole;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE,
			name = "role",
			propOrder = {"documentation", "description", "roles"})
public class CustomerRoleMetaData extends NamedMetaData implements CustomerRole {
	private static final long serialVersionUID = -7824222183005636350L;

	private String description;
	private List<CustomerModuleRoleMetaData> roles = new ArrayList<>();
	private String documentation;
	
	@Override
	public String getDescription() {
		return description;
	}

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = true)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}

	@XmlElementWrapper(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "roles")
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "role", required = true)
	public List<CustomerModuleRoleMetaData> getRoles() {
		return roles;
	}

	@Override
	public String getDocumentation() {
		return documentation;
	}

	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}
	
	public List<Role> getModuleRoles(Customer customer) {
		List<Role> result = new ArrayList<>(roles.size());
		for (CustomerModuleRoleMetaData role : roles) {
			Module module = customer.getModule(role.getModuleName());
			result.add(module.getRole(role.getName()));
		}
		return result;
	}
}
