package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * Content Restrictions are specified in user roles.
 * It is assumed that most uses will want a completely open textual searching
 * mechanism so as to be most useful - thus we specify restrictions instead of permissions.
 * 
 * If an attribute is restricted it will not be streamed from the 
 * CustomerResourceServlet.
 * 
 * @author Mike
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class ContentRestriction {
	private String attributeName;
	private String documentName;

	public String getAttributeName() {
		return attributeName;
	}

	@XmlAttribute(name = "attribute", required = true)
	public void setAttributeName(String attributeName) {
		this.attributeName = UtilImpl.processStringValue(attributeName);
	}

	public String getDocumentName() {
		return documentName;
	}

	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}
}
