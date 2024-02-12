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
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "modelAggregate")
public class ViewModelAggregateUserAccessMetaData extends ViewUserAccessMetaData {
	private static final long serialVersionUID = -3977408737246074194L;

	private String modelName;

	public String getModelName() {
		return modelName;
	}

	@XmlAttribute(name = "model", required = true)
	public void setModelName(String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}
	
	@Override
	public void validate(String metaDataName, Module module) {
		if (modelName == null) {
			throw new MetaDataException(metaDataName + " : [modelName] is required for all modelAggregate user accesses");
		}
		// NB can't validate modelName until second pass validation in LocalDesignRepository.validateViewForGenerateDomain()
	}
	
	@Override
	public UserAccess toUserAccess(String moduleName, String documentName) {
		return UserAccess.modelAggregate(moduleName, documentName, modelName);
	}
}
