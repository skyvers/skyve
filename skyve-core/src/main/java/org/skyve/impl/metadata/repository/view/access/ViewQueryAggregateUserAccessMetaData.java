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
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "queryAggregateAccess")
public class ViewQueryAggregateUserAccessMetaData extends ViewUserAccessMetaData {
	private static final long serialVersionUID = -1230722966934378861L;

	private String queryName;

	public String getQueryName() {
		return queryName;
	}

	@XmlAttribute(name = "query", required = true)
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}
	
	@Override
	public void validate(String metaDataName, Module module) {
		if (queryName == null) {
			throw new MetaDataException(metaDataName + " : [query] is required for all queryAggregate user accesses.");
		}
		if (module.getMetaDataQuery(queryName) == null) {
			String moduleName = module.getName();
			throw new MetaDataException(metaDataName + " : [query] " + queryName + " does not exist for user access " + toUserAccess(moduleName, null).toString() + " in module " + moduleName);
		}
	}

	@Override
	public UserAccess toUserAccess(String moduleName, String documentName) {
		return UserAccess.queryAggregate(moduleName, queryName);
	}
}
