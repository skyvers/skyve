package org.skyve.impl.metadata.model.document;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.UniqueConstraint;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * This class is used for collection unique constraints and for document unique constraints.
 * Document Unique Constraints are converted from repository.documentUniqueConstraint
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, 
			name = "CollectionUniqueConstraint", 
			propOrder = {"name", "description", "message", "fieldNames"})
public class UniqueConstraintImpl implements UniqueConstraint {
	private static final long serialVersionUID = -1108913028844957459L;

	private String name;
	private DocumentScope scope;
	private String description;
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
	public String getDescription() {
		return description;
	}

	@XmlAttribute
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
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
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "message", required = true)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setMessage(String message) {
		this.message = UtilImpl.processStringValue(message);
	}

	/**
	 * @return Returns the fieldNames.
	 */
	@Override
	@XmlElementWrapper(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "fieldReferences")
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "ref", required = true)
	public List<String> getFieldNames() {
		return fieldNames;
	}
}
