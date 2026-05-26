package org.skyve.impl.metadata.repository.view.access;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.user.UserAccess;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <singularAccess>} access constraint on
 * a view; restricts the view to users with singular (edit-view) access to the
 * named document.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ViewDocumentAggregateUserAccessMetaData
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "singularAccess")
public class ViewSingularUserAccessMetaData extends ViewDocumentAggregateUserAccessMetaData {
	private static final long serialVersionUID = 2888202790675893096L;
	
	@Override
	public UserAccess toUserAccess(String moduleName, String documentName) {
		return UserAccess.singular(moduleName, getDocumentName());
	}
}
