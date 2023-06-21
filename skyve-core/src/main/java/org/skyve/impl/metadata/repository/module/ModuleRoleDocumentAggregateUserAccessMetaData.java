package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.UserAccess;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "documentAggregate")
public class ModuleRoleDocumentAggregateUserAccessMetaData extends ModuleRoleUserAccessMetaData {
	private static final long serialVersionUID = -9055003769635277281L;
	
	private String documentName;

	public String getDocumentName() {
		return documentName;
	}

	@XmlAttribute(name = "document", required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}
	
	@Override
	public void validate(String metaDataName, String roleName, Module module) {
		if (documentName == null) {
			throw new MetaDataException(metaDataName + " : [documentName] is required for all documentAggregate, modelAggregate, previousComplete & singular user accesses defined in module role " + roleName);
		}
		if (! module.getDocumentRefs().keySet().contains(documentName)) {
			throw new MetaDataException(metaDataName + " : [documentName] " + documentName + " does not exist for user access " + toUserAccess(module.getName()).toString() + " in module role " + roleName);
		}
	}

	@Override
	public UserAccess toUserAccess(String moduleName) {
		return UserAccess.documentAggregate(moduleName, documentName);
	}
}
