package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.UserAccess;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <documentAggregateAccess>} user-access
 * grant in a module role.
 *
 * <p>Grants access to aggregate chart/report views derived from a named document.
 * Extends {@link ModuleRoleUserAccessMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ModuleRoleUserAccessMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "documentAggregateAccess")
public class ModuleRoleDocumentAggregateUserAccessMetaData extends ModuleRoleUserAccessMetaData {
	private static final long serialVersionUID = -9055003769635277281L;
	
	private String documentName;

	/**
	 * Returns the target document name for this aggregate-access grant.
	 *
	 * @return the target document name
	 */
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * Sets the target document name for this aggregate-access grant.
	 *
	 * <p>Side effects: normalises the supplied value with
	 * {@link UtilImpl#processStringValue(String)} before storing it.
	 *
	 * @param documentName the target document name; blank values become {@code null}
	 */
	@XmlAttribute(name = "document", required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}
	
	/**
	 * Validates that the configured document reference exists in the owning module.
	 *
	 * @param metaDataName the source metadata identifier used in validation errors
	 * @param roleName the owning role name used in validation errors
	 * @param module the module used to resolve document references
	 * @throws MetaDataException if {@code document} is missing or does not resolve in {@code module}
	 */
	@Override
	public void validate(String metaDataName, String roleName, Module module) {
		if (documentName == null) {
			throw new MetaDataException(metaDataName + " : [document] is required for all documentAggregate, modelAggregate, previousComplete & singular user accesses defined in module role " + roleName);
		}
		if (! module.getDocumentRefs().keySet().contains(documentName)) {
			throw new MetaDataException(metaDataName + " : [document] " + documentName + " does not exist for user access " + toUserAccess(module.getName()).toString() + " in module role " + roleName);
		}
	}

	/**
	 * Creates a document-aggregate user access descriptor for the configured document.
	 *
	 * @param moduleName the owning module name
	 * @return the runtime document-aggregate user access
	 */
	@Override
	public UserAccess toUserAccess(String moduleName) {
		return UserAccess.documentAggregate(moduleName, documentName);
	}
}
