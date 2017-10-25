package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE,
			propOrder = {"documentation", "description", "privileges"})
public class Role extends NamedMetaData {
	private static final long serialVersionUID = -7824222183005636350L;

	private String description;
	private List<DocumentPrivilege> privileges = new ArrayList<>();
	private String documentation;
	
	public String getDescription() {
		return description;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, required = true)
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}

	@XmlElementWrapper(namespace = XMLMetaData.MODULE_NAMESPACE, name = "privileges")
	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "document", required = true)
	public List<DocumentPrivilege> getPrivileges() {
		return privileges;
	}

	public String getDocumentation() {
		return documentation;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}
}
