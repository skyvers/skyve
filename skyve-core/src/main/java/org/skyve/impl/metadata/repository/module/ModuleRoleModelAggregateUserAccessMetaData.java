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
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "modelAggregate")
public class ModuleRoleModelAggregateUserAccessMetaData extends ModuleRoleDocumentAggregateUserAccessMetaData {
	private static final long serialVersionUID = 950718682785023214L;

	private String modelName;

	public String getModelName() {
		return modelName;
	}

	@XmlAttribute(name = "model", required = true)
	public void setModelName(String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}
	
	@Override
	public void validate(String metaDataName, String roleName, Module module) {
		super.validate(metaDataName, roleName, module);
		if (modelName == null) {
			throw new MetaDataException(metaDataName + " : [modelName] is required for all modelAggregate user accesses defined in module role " + roleName);
		}
		// NB can't validate modelName exists until second pass validation in LocalDesignRepository.validateModuleForGenerateDomain()
	}
	
	@Override
	public UserAccess toUserAccess(String moduleName) {
		return UserAccess.modelAggregate(moduleName, getDocumentName(), modelName);
	}
}
