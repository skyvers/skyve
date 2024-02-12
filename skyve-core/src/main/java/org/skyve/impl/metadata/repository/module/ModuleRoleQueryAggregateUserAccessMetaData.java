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
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "queryAggregate")
public class ModuleRoleQueryAggregateUserAccessMetaData extends ModuleRoleUserAccessMetaData {
	private static final long serialVersionUID = 7011821460848457822L;

	private String queryName;

	public String getQueryName() {
		return queryName;
	}

	@XmlAttribute(name = "query", required = true)
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}
	
	@Override
	public void validate(String metaDataName, String roleName, Module module) {
		if (queryName == null) {
			throw new MetaDataException(metaDataName + " : [queryName] is required for all queryAggregate user accesses defined in module role " + roleName);
		}
		if (module.getMetaDataQuery(queryName) == null) {
			throw new MetaDataException(metaDataName + " : [queryName] " + queryName + " does not exist for user access " + toUserAccess(module.getName()).toString() + " in module role " + roleName);
		}
	}

	@Override
	public UserAccess toUserAccess(String moduleName) {
		return UserAccess.queryAggregate(moduleName, queryName);
	}
}
