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
 * JAXB-annotated descriptor for a {@code <modelAggregateAccess>} user-access grant
 * in a module role.
 *
 * <p>Grants access to aggregate views rendered by a named
 * {@link org.skyve.metadata.module.Module.DocumentAggregateChartModel} model.
 * Extends {@link ModuleRoleDocumentAggregateUserAccessMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ModuleRoleDocumentAggregateUserAccessMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "modelAggregateAccess")
public class ModuleRoleModelAggregateUserAccessMetaData extends ModuleRoleDocumentAggregateUserAccessMetaData {
	private static final long serialVersionUID = 950718682785023214L;

	private String modelName;

	/**
	 * Returns the aggregate model identifier for this access grant.
	 *
	 * @return the aggregate model name
	 */
	public String getModelName() {
		return modelName;
	}

	/**
	 * Sets the aggregate model identifier for this access grant.
	 *
	 * <p>Side effects: normalises the supplied value with
	 * {@link UtilImpl#processStringValue(String)} before storing it.
	 *
	 * @param modelName the aggregate model name; blank values become {@code null}
	 */
	@XmlAttribute(name = "model", required = true)
	public void setModelName(String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}
	
	/**
	 * Validates this model-aggregate access declaration.
	 *
	 * <p>Validates common document requirements via {@code super.validate(...)} and
	 * confirms a model name is supplied.
	 *
	 * @param metaDataName the source metadata identifier used in validation errors
	 * @param roleName the owning role name used in validation errors
	 * @param module the module used to resolve inherited document references
	 * @throws MetaDataException if required attributes are missing
	 */
	@Override
	public void validate(String metaDataName, String roleName, Module module) {
		super.validate(metaDataName, roleName, module);
		if (modelName == null) {
			throw new MetaDataException(metaDataName + " : [model] is required for all modelAggregate user accesses defined in module role " + roleName);
		}
		// NB can't validate modelName exists until second pass validation in LocalDesignRepository.validateModuleForGenerateDomain()
	}
	
	/**
	 * Creates a model-aggregate user access descriptor for the configured model.
	 *
	 * @param moduleName the owning module name
	 * @return the runtime model-aggregate user access
	 */
	@Override
	public UserAccess toUserAccess(String moduleName) {
		return UserAccess.modelAggregate(moduleName, getDocumentName(), modelName);
	}
}
