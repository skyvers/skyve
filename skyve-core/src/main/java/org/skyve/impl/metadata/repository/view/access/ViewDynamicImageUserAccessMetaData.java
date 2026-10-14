package org.skyve.impl.metadata.repository.view.access;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.UserAccess;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <dynamicImageAccess>} access constraint
 * on a view; restricts the view to users with dynamic-image rendering access.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ViewUserAccessMetaData
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "dynamicImageAccess")
public class ViewDynamicImageUserAccessMetaData extends ViewUserAccessMetaData {
	private static final long serialVersionUID = 8648081821441377655L;

	private String imageName;

	public @Nonnull String getImageName() {
		return imageName;
	}

	@XmlAttribute(name = "image", required = true)
	public void setImageName(@Nonnull String imageName) {
		this.imageName = UtilImpl.processStringValue(imageName);
	}
	
	@Override
	public void validate(@Nonnull String metaDataName, @Nonnull Module module) {
		if (imageName == null) {
			throw new MetaDataException(metaDataName + " : [image] is required for all dynamic image user accesses.");
		}
		// NB can't validate imageName until second pass validation in LocalDesignRepository.validateViewForGenerateDomain()
	}

	@Override
	public @Nonnull UserAccess toUserAccess(@Nonnull String moduleName, @Nonnull String documentName) {
		return UserAccess.dynamicImage(moduleName, documentName, imageName);
	}
}
