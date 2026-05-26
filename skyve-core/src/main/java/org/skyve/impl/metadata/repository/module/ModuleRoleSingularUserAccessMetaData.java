package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.user.UserAccess;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <singularAccess>} user-access grant in
 * a module role.
 *
 * <p>Grants access to the edit (singular) view of a specific document instance.
 * Extends {@link ModuleRoleDocumentAggregateUserAccessMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ModuleRoleDocumentAggregateUserAccessMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "singularAccess")
public class ModuleRoleSingularUserAccessMetaData extends ModuleRoleDocumentAggregateUserAccessMetaData {
	private static final long serialVersionUID = 2888202790675893096L;
	
	@Override
	public UserAccess toUserAccess(String moduleName) {
		return UserAccess.singular(moduleName, getDocumentName());
	}
}
