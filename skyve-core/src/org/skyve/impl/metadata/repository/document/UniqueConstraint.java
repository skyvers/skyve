package org.skyve.impl.metadata.repository.document;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLUtil;
import org.skyve.metadata.model.document.UniqueConstraint.DocumentScope;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE, 
			name = "DocumentUniqueConstraint", 
			propOrder = {"scope", "description", "message", "fieldReferences"})
public class UniqueConstraint extends NamedMetaData {
	private String description;
	private DocumentScope scope;
	private String message;
	private List<FieldReference> fieldReferences = new ArrayList<>();
	
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

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE, required = true)
	public void setMessage(String message) {
		this.message = UtilImpl.processStringValue(message);
	}

	@XmlElementWrapper(namespace = XMLUtil.DOCUMENT_NAMESPACE, name = "fieldReferences")
	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE, name = "ref", required = true)
	public List<FieldReference> getFieldReferences() {
		return fieldReferences;
	}
}
