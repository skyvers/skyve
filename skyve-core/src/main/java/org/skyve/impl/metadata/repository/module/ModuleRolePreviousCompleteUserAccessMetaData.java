package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.UserAccess;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "previousComplete")
public class ModuleRolePreviousCompleteUserAccessMetaData extends ModuleRoleDocumentAggregateUserAccessMetaData {
	private static final long serialVersionUID = -8480621023456685741L;

	private String binding;

	public String getBinding() {
		return binding;
	}

	@XmlAttribute(name = "binding", required = true)
	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}
	
	@Override
	public void validate(String metaDataName, String roleName, Module module) {
		super.validate(metaDataName, roleName, module);
		if (binding == null) {
			throw new MetaDataException(metaDataName + " : [binding] is required for all previousComplete user accesses defined in module role " + roleName);
		}
		// NB can't validate binding until second pass validation in LocalDesignRepository.validateModuleForGenerateDomain()
	}

	@Override
	public UserAccess toUserAccess(String moduleName) {
		return UserAccess.previousComplete(moduleName, getDocumentName(), binding);
	}
}
