package org.skyve.wildcat.metadata.repository.document;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.model.document.UniqueConstraint.DocumentScope;
import org.skyve.wildcat.metadata.repository.NamedMetaData;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE, 
			name = "DocumentUniqueConstraint", 
			propOrder = {"scope", "shortDescription", "message", "fieldReferences"})
public class UniqueConstraint extends NamedMetaData {
	private String shortDescription;
	private DocumentScope scope;
	private String message;
	private List<FieldReference> fieldReferences = new ArrayList<>();
	
	public String getShortDescription() {
		return shortDescription;
	}

	@XmlAttribute
	public void setShortDescription(String shortDescription) {
		this.shortDescription = UtilImpl.processStringValue(shortDescription);
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
