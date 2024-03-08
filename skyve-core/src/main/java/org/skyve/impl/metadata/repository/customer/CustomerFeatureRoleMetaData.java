package org.skyve.impl.metadata.repository.customer;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "customerFeature")
public class CustomerFeatureRoleMetaData extends NamedMetaData {
	private static final long serialVersionUID = 4695670321378511439L;
	
	private String moduleName;

	public String getModuleName() {
		return moduleName;
	}

	@XmlAttribute(name = "module")
	public void setModuleName(String moduleName) {
		this.moduleName = Util.processStringValue(moduleName);
	}
}
