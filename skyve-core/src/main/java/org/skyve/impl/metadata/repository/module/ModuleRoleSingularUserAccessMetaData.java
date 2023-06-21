package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.user.UserAccess;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "singular")
public class ModuleRoleSingularUserAccessMetaData extends ModuleRoleDocumentAggregateUserAccessMetaData {
	private static final long serialVersionUID = 2888202790675893096L;
	
	@Override
	public UserAccess toUserAccess(String moduleName) {
		return UserAccess.singular(moduleName, getDocumentName());
	}
}
