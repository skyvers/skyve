package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.UserAccess;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "dynamicImageAccess")
public class ModuleRoleDynamicImageUserAccessMetaData extends ModuleRoleDocumentAggregateUserAccessMetaData {
	private static final long serialVersionUID = 4028762159693882009L;

	private String imageName;

	public String getImageName() {
		return imageName;
	}

	@XmlAttribute(name = "image", required = true)
	public void setImageName(String imageName) {
		this.imageName = UtilImpl.processStringValue(imageName);
	}
	
	@Override
	public void validate(String metaDataName, String roleName, Module module) {
		super.validate(metaDataName, roleName, module);
		if (imageName == null) {
			throw new MetaDataException(metaDataName + " : [image] is required for all dynamicImage user accesses defined in module role " + roleName);
		}
		// NB can't validate imageName exists until second pass validation in LocalDesignRepository.validateModuleForGenerateDomain()
	}
	
	@Override
	public UserAccess toUserAccess(String moduleName) {
		return UserAccess.dynamicImage(moduleName, getDocumentName(), imageName);
	}
}
