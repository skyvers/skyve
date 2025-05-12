package org.skyve.impl.metadata.repository.module;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Content Permissions are specified in user roles.
 * It is assumed that most uses will want a completely open textual searching
 * mechanism so as to be most useful - but we can specify permissions instead of restrictions,
 * when the entire document is not readable or the document is transient.
 * 
 * If an attribute is permitted it will not be streamed from the 
 * CustomerResourceServlet.
 * 
 * @author Mike
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class ContentPermission implements SerializableMetaData {
	private static final long serialVersionUID = 7523462852059026359L;

	private String attributeName;
	private String documentName;

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

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

	public Map<String, String> getProperties() {
		return properties;
	}
	
	/**
	 * This is called by convert
	 */
	@XmlTransient
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}
}
