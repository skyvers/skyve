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
 * JAXB-annotated descriptor for a {@code <contentAccess>} user-access grant in
 * a module role.
 *
 * <p>Grants access to download or view binary content (documents/images) stored
 * against a named document attribute.  Extends
 * {@link ModuleRoleDocumentAggregateUserAccessMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ModuleRoleDocumentAggregateUserAccessMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "contentAccess")
public class ModuleRoleContentUserAccessMetaData extends ModuleRoleDocumentAggregateUserAccessMetaData {
	private static final long serialVersionUID = -600949529839627446L;

	private String binding;

	/**
	 * Returns the document binding path for the content attribute.
	 *
	 * @return the content binding path
	 */
	public String getBinding() {
		return binding;
	}

	/**
	 * Sets the document binding path for the content attribute.
	 *
	 * <p>Side effects: normalises the supplied value with
	 * {@link UtilImpl#processStringValue(String)} before storing it.
	 *
	 * @param binding the content binding path; blank values become {@code null}
	 */
	@XmlAttribute(name = "binding", required = true)
	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}
	
	/**
	 * Validates this content-access declaration.
	 *
	 * <p>Validates common document requirements via {@code super.validate(...)} and
	 * confirms a content binding is supplied.
	 *
	 * @param metaDataName the source metadata identifier used in validation errors
	 * @param roleName the owning role name used in validation errors
	 * @param module the module used to resolve inherited document references
	 * @throws MetaDataException if required attributes are missing
	 */
	@Override
	public void validate(String metaDataName, String roleName, Module module) {
		super.validate(metaDataName, roleName, module);
		if (binding == null) {
			throw new MetaDataException(metaDataName + " : [binding] is required for all content user accesses defined in module role " + roleName);
		}
		// NB can't validate binding until second pass validation in LocalDesignRepository.validateModuleForGenerateDomain()
	}

	/**
	 * Creates a content user access descriptor for the configured document binding.
	 *
	 * @param moduleName the owning module name
	 * @return the runtime content user access
	 */
	@Override
	public UserAccess toUserAccess(String moduleName) {
		return UserAccess.content(moduleName, getDocumentName(), binding);
	}
}
