package org.skyve.impl.metadata.repository.customer;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a role granted to a customer within a module.
 *
 * <p>Used inside {@link CustomerModuleMetaData} to restrict or grant module roles
 * for a specific customer tenant.  Extends {@link NamedMetaData} to carry the
 * role name.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see CustomerModuleMetaData
 */
@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "moduleRole")
public class CustomerModuleRoleMetaData extends NamedMetaData {
	private static final long serialVersionUID = 5588340993333875447L;

	private String moduleName;
	
	public String getModuleName() {
		return moduleName;
	}

	@XmlAttribute(name = "module", required = true)
	public void setModuleName(String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}
}
