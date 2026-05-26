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
 * JAXB-annotated descriptor for a {@code <previousCompleteAccess>} user-access
 * grant in a module role.
 *
 * <p>Grants access to view previous-complete snapshots of records undergoing
 * a workflow-style status transition on a named document.  Extends
 * {@link ModuleRoleDocumentAggregateUserAccessMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ModuleRoleDocumentAggregateUserAccessMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "previousCompleteAccess")
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
