package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.user.UserAccess;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Abstract JAXB base for user-access grant elements within a module role.
 *
 * <p>Concrete subclasses ({@link ModuleRoleDocumentAggregateUserAccessMetaData},
 * {@link ModuleRoleQueryAggregateUserAccessMetaData}, {@link ModuleRoleSingularUserAccessMetaData},
 * etc.) specify the resource type and access scope.  An optional
 * {@link ModuleRoleUserAccessUxUiMetadata} list restricts the grant to specific
 * UX/UI scopes.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ModuleRoleMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public abstract class ModuleRoleUserAccessMetaData implements SerializableMetaData {
	private static final long serialVersionUID = -6274893418024546257L;

	private List<ModuleRoleUserAccessUxUiMetadata> uxuis = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "uxui")
	public List<ModuleRoleUserAccessUxUiMetadata> getUxuis() {
		return uxuis;
	}
	
	public abstract void validate(String metaDataName, String roleName, Module module);
	public abstract UserAccess toUserAccess(String moduleName);
}
