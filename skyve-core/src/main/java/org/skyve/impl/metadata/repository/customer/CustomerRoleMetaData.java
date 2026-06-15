package org.skyve.impl.metadata.repository.customer;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
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

/**
 * JAXB-annotated descriptor for a {@code <role>} element within the customer roles
 * section of a {@code customer.xml} file.
 *
 * <p>Defines a named composite role that aggregates module-level roles into a
 * single assignable customer role.  Implements {@link CustomerRole} so it can
 * be returned directly from the runtime {@link Customer}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see CustomerRolesMetaData
 * @see org.skyve.metadata.customer.CustomerRole
 */
@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE,
			name = "role",
			propOrder = {"documentation", "description", "roles", "properties"})
public class CustomerRoleMetaData extends NamedMetaData implements CustomerRole {
	private static final long serialVersionUID = -7824222183005636350L;

	private String description;
	private List<CustomerModuleRoleMetaData> roles = new ArrayList<>();
	private String documentation;
	
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();
	
	/**
	 * Returns the human-readable role description.
	 *
	 * @return role description text, or {@code null}
	 */
	@Override
	public String getDescription() {
		return description;
	}

	/**
	 * Sets the human-readable role description.
	 *
	 * @param description role description text
	 */
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, required = true)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}

	/**
	 * Returns module role mappings that make up this composite customer role.
	 *
	 * @return mutable list of module role metadata
	 */
	@XmlElementWrapper(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "roles")
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "role", required = true)
	public List<CustomerModuleRoleMetaData> getRoles() {
		return roles;
	}

	/**
	 * Returns long-form documentation for this role.
	 *
	 * @return role documentation text, or {@code null}
	 */
	@Override
	public String getDocumentation() {
		return documentation;
	}

	/**
	 * Sets long-form documentation for this role.
	 *
	 * @param documentation role documentation text
	 */
	@XmlElement(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}
	
	/**
	 * Returns decorator properties defined on this customer role descriptor.
	 *
	 * @return mutable role property map
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
	
	/**
	 * Resolves this customer role's configured module roles against a customer.
	 *
	 * @param customer the customer providing module definitions
	 * @return resolved runtime module roles in declaration order
	 */
	public List<Role> getModuleRoles(Customer customer) {
		List<Role> result = new ArrayList<>(roles.size());
		for (CustomerModuleRoleMetaData role : roles) {
			Module module = customer.getModule(role.getModuleName());
			result.add(module.getNullSafeRole(role.getName()));
		}
		return result;
	}
}
