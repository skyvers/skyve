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
 * JAXB-annotated descriptor for a {@code <dynamicImageAccess>} user-access grant
 * in a module role.
 *
 * <p>Grants access to server-rendered dynamic images (chart thumbnails, generated
 * diagrams) for a named document.  Extends
 * {@link ModuleRoleDocumentAggregateUserAccessMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ModuleRoleDocumentAggregateUserAccessMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "dynamicImageAccess")
public class ModuleRoleDynamicImageUserAccessMetaData extends ModuleRoleDocumentAggregateUserAccessMetaData {
	private static final long serialVersionUID = 4028762159693882009L;

	private String imageName;

	/**
	 * Returns the dynamic-image identifier for this access grant.
	 *
	 * @return the dynamic-image name
	 */
	public String getImageName() {
		return imageName;
	}

	/**
	 * Sets the dynamic-image identifier for this access grant.
	 *
	 * <p>Side effects: normalises the supplied value with
	 * {@link UtilImpl#processStringValue(String)} before storing it.
	 *
	 * @param imageName the dynamic-image name; blank values become {@code null}
	 */
	@XmlAttribute(name = "image", required = true)
	public void setImageName(String imageName) {
		this.imageName = UtilImpl.processStringValue(imageName);
	}
	
	/**
	 * Validates this dynamic-image access declaration.
	 *
	 * <p>Validates common document requirements via {@code super.validate(...)} and
	 * confirms an image binding name is supplied.
	 *
	 * @param metaDataName the source metadata identifier used in validation errors
	 * @param roleName the owning role name used in validation errors
	 * @param module the module used to resolve inherited document references
	 * @throws MetaDataException if required attributes are missing
	 */
	@Override
	public void validate(String metaDataName, String roleName, Module module) {
		super.validate(metaDataName, roleName, module);
		if (imageName == null) {
			throw new MetaDataException(metaDataName + " : [image] is required for all dynamicImage user accesses defined in module role " + roleName);
		}
		// NB can't validate imageName exists until second pass validation in LocalDesignRepository.validateModuleForGenerateDomain()
	}
	
	/**
	 * Creates a dynamic-image user access descriptor for the configured document and image.
	 *
	 * @param moduleName the owning module name
	 * @return the runtime dynamic-image user access
	 */
	@Override
	public UserAccess toUserAccess(String moduleName) {
		return UserAccess.dynamicImage(moduleName, getDocumentName(), imageName);
	}
}
