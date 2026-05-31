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
 * JAXB-annotated descriptor for a {@code <queryAggregateAccess>} user-access
 * grant in a module role.
 *
 * <p>Grants access to aggregate chart/report views driven by a named module query.
 * Extends {@link ModuleRoleUserAccessMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ModuleRoleUserAccessMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "queryAggregateAccess")
public class ModuleRoleQueryAggregateUserAccessMetaData extends ModuleRoleUserAccessMetaData {
	private static final long serialVersionUID = 7011821460848457822L;

	private String queryName;

	/**
	 * Returns the target query name for this aggregate-access grant.
	 *
	 * @return the target query name
	 */
	public String getQueryName() {
		return queryName;
	}

	/**
	 * Sets the target query name for this aggregate-access grant.
	 *
	 * <p>Side effects: normalises the supplied value with
	 * {@link UtilImpl#processStringValue(String)} before storing it.
	 *
	 * @param queryName the target query name; blank values become {@code null}
	 */
	@XmlAttribute(name = "query", required = true)
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}
	
	/**
	 * Validates that the configured query reference exists in the owning module.
	 *
	 * @param metaDataName the source metadata identifier used in validation errors
	 * @param roleName the owning role name used in validation errors
	 * @param module the module used to resolve query references
	 * @throws MetaDataException if {@code query} is missing or does not resolve in {@code module}
	 */
	@Override
	public void validate(String metaDataName, String roleName, Module module) {
		if (queryName == null) {
			throw new MetaDataException(metaDataName + " : [query] is required for all queryAggregate user accesses defined in module role " + roleName);
		}
		if (module.getMetaDataQuery(queryName) == null) {
			throw new MetaDataException(metaDataName + " : [query] " + queryName + " does not exist for user access " + toUserAccess(module.getName()).toString() + " in module role " + roleName);
		}
	}

	/**
	 * Creates a query-aggregate user access descriptor for the configured query.
	 *
	 * @param moduleName the owning module name
	 * @return the runtime query-aggregate user access
	 */
	@Override
	public UserAccess toUserAccess(String moduleName) {
		return UserAccess.queryAggregate(moduleName, queryName);
	}
}
