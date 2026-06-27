package org.skyve.impl.metadata.repository.customer;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a customer-specific feature role.
 *
 * <p>Associates a named feature flag with a customer, allowing tenants to opt-in
 * or opt-out of optional framework features through the {@code customer.xml}
 * descriptor.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "customerFeature")
public class CustomerFeatureRoleMetaData extends NamedMetaData {
	private static final long serialVersionUID = 4695670321378511439L;
	
	private String moduleName;

	public String getModuleName() {
		return moduleName;
	}

	@XmlAttribute(name = "module")
	public void setModuleName(String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}
}
