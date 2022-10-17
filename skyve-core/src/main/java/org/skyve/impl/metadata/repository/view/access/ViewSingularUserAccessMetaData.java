package org.skyve.impl.metadata.repository.view.access;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.user.UserAccess;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "singular")
public class ViewSingularUserAccessMetaData extends ViewDocumentAggregateUserAccessMetaData {
	private static final long serialVersionUID = 2888202790675893096L;
	
	@Override
	public UserAccess toUserAccess(String moduleName, String documentName) {
		return UserAccess.singular(moduleName, getDocumentName());
	}
}
