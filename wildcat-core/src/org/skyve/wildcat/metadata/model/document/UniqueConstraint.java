package org.skyve.wildcat.metadata.model.document;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

/**
 * This class is used for collection unique constraints and for document unique constraints.
 * Document Unique Constraints are converted from repository.documentUniqueConstraint
 */
@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE, 
			name = "CollectionUniqueConstraint", 
			propOrder = {"name", "shortDescription", "message", "fieldNames"})
public class UniqueConstraint implements org.skyve.metadata.model.document.UniqueConstraint {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -1108913028844957459L;

	private String name;
	private DocumentScope scope;
	private String shortDescription;
	private String message;
	private List<String> fieldNames = new ArrayList<>();
	
	/**
	 * @return Returns the name.
	 */
	@Override
	public String getName() {
		return name;
	}

	/**
	 * @param name The name to set.
	 */
	@XmlAttribute(name = "name", required = true)
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}

	@Override
	public DocumentScope getScope() {
		return scope;
	}

	@XmlTransient // not used in collection unique constraints
	public void setScope(DocumentScope scope) {
		this.scope = scope;
	}

	@Override
	public String getShortDescription() {
		return shortDescription;
	}

	@XmlAttribute
	public void setShortDescription(String shortDescription) {
		this.shortDescription = UtilImpl.processStringValue(shortDescription);
	}

	/**
	 * @return Returns the message.
	 */
	@Override
	public String getMessage() {
		return message;
	}

	/**
	 * @param message The message to set.
	 */
	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE, name = "message", required = true)
	public void setMessage(String message) {
		this.message = UtilImpl.processStringValue(message);
	}

	/**
	 * @return Returns the fieldNames.
	 */
	@Override
	@XmlElementWrapper(namespace = XMLUtil.DOCUMENT_NAMESPACE, name = "fieldReferences")
	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE, name = "ref", required = true)
	public List<String> getFieldNames() {
		return fieldNames;
	}
}
