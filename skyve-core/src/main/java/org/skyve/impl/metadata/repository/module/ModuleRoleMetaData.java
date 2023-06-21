package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE,
			name = "role",
			propOrder = {"documentation", "description", "privileges", "accesses"})
public class ModuleRoleMetaData extends NamedMetaData {
	private static final long serialVersionUID = -7824222183005636350L;

	private String documentation;
	private String description;
	private List<DocumentPrivilegeMetaData> privileges = new ArrayList<>();
	private List<ModuleRoleUserAccessMetaData> accesses = new ArrayList<>();

	
	public String getDocumentation() {
		return documentation;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}

	public String getDescription() {
		return description;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, required = true)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}

	@XmlElementWrapper(namespace = XMLMetaData.MODULE_NAMESPACE, name = "privileges")
	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "document", required = true)
	public List<DocumentPrivilegeMetaData> getPrivileges() {
		return privileges;
	}

	@XmlElementWrapper(namespace = XMLMetaData.MODULE_NAMESPACE, name = "accesses")
	@XmlElementRefs({@XmlElementRef(type = ModuleRoleSingularUserAccessMetaData.class), 
						@XmlElementRef(type = ModuleRoleDocumentAggregateUserAccessMetaData.class),
						@XmlElementRef(type = ModuleRoleQueryAggregateUserAccessMetaData.class),
						@XmlElementRef(type = ModuleRoleModelAggregateUserAccessMetaData.class),
						@XmlElementRef(type = ModuleRolePreviousCompleteUserAccessMetaData.class)})
	public List<ModuleRoleUserAccessMetaData> getAccesses() {
		return accesses;
	}
}
