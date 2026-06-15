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

	/**
	 * Returns the UX/UI scope restrictions attached to this access declaration.
	 *
	 * <p>Returns the live mutable list used by JAXB population and later validation.
	 * An empty list means the access applies to all UX/UI contexts.
	 *
	 * @return the mutable backing list of UX/UI restrictions, never {@code null}
	 */
	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "uxui")
	public List<ModuleRoleUserAccessUxUiMetadata> getUxuis() {
		return uxuis;
	}
	
	/**
	 * Validates this access declaration against the module metadata.
	 *
	 * @param metaDataName the source metadata identifier used in validation errors
	 * @param roleName the owning role name used in validation errors
	 * @param module the resolved module containing documents, queries, and related resources
	 * @throws org.skyve.metadata.MetaDataException if required attributes are missing or invalid
	 */
	public abstract void validate(String metaDataName, String roleName, Module module);

	/**
	 * Converts this metadata declaration into a runtime {@link UserAccess} value.
	 *
	 * @param moduleName the owning module name for access types scoped to the current module
	 * @return the runtime user-access descriptor
	 */
	public abstract UserAccess toUserAccess(String moduleName);
}
