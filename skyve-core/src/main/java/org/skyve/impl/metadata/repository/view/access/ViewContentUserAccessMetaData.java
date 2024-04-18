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
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "contentAccess")
public class ViewContentUserAccessMetaData extends ViewUserAccessMetaData {
	private static final long serialVersionUID = 4118889847887224975L;

	private String binding;

	public String getBinding() {
		return binding;
	}

	@XmlAttribute(name = "binding", required = true)
	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}
	
	@Override
	public void validate(String metaDataName, Module module) {
		if (binding == null) {
			throw new MetaDataException(metaDataName + " : [binding] is required for all content user accesses.");
		}
		// NB can't validate binding until second pass validation in LocalDesignRepository.validateViewForGenerateDomain()
	}

	@Override
	public UserAccess toUserAccess(String moduleName, String documentName) {
		return UserAccess.content(moduleName, documentName, binding);
	}
}
