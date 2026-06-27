package org.skyve.impl.metadata.repository.document;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.UniqueConstraint.DocumentScope;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated descriptor for a {@code <uniqueConstraint>} element in a document
 * XML file.
 *
 * <p>Declares a named composite uniqueness constraint on a document, specifying the
 * scope (user / customer / global), a violation message, and the ordered list of
 * {@link FieldReference} attribute paths that form the key.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see FieldReference
 * @see org.skyve.metadata.model.document.Document
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, 
			name = "DocumentUniqueConstraint", 
			propOrder = {"scope", "description", "message", "fieldReferences", "properties"})
public class UniqueConstraint extends NamedMetaData {
	private static final long serialVersionUID = -5802589971987905636L;

	private String description;
	private DocumentScope scope;
	private String message;
	private List<FieldReference> fieldReferences = new ArrayList<>();
	
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();
	
	public String getDescription() {
		return description;
	}

	@XmlAttribute
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}

	public DocumentScope getScope() {
		return scope;
	}

	@XmlAttribute(required = true)
	public void setScope(DocumentScope scope) {
		this.scope = scope;
	}

	public String getMessage() {
		return message;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = true)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setMessage(String message) {
		this.message = UtilImpl.processStringValue(message);
	}

	@XmlElementWrapper(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "fieldReferences")
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "ref", required = true)
	public List<FieldReference> getFieldReferences() {
		return fieldReferences;
	}
	
	public Map<String, String> getProperties() {
		return properties;
	}
}
