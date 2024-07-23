package org.skyve.impl.metadata.repository.view.access;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.UserAccess;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "dynamicImageAccess")
public class ViewDynamicImageUserAccessMetaData extends ViewUserAccessMetaData {
	private static final long serialVersionUID = 8648081821441377655L;

	private String imageName;

	public String getImageName() {
		return imageName;
	}

	@XmlAttribute(name = "image", required = true)
	public void setImageName(String imageName) {
		this.imageName = UtilImpl.processStringValue(imageName);
	}
	
	@Override
	public void validate(String metaDataName, Module module) {
		if (imageName == null) {
			throw new MetaDataException(metaDataName + " : [image] is required for all dynamic image user accesses.");
		}
		// NB can't validate imageName until second pass validation in LocalDesignRepository.validateViewForGenerateDomain()
	}

	@Override
	public UserAccess toUserAccess(String moduleName, String documentName) {
		return UserAccess.dynamicImage(moduleName, documentName, imageName);
	}
}
