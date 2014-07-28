package org.skyve.wildcat.metadata.model.document;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE, propOrder = {"requiredBool", "type"})
public class Association extends Reference implements org.skyve.metadata.model.document.Association {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -2839713495173145591L;

	private boolean required;
	private AssociationType type;

	public Association() {
		setAttributeType(AttributeType.association);
	}

	@Override
	public AssociationType getType() {
		return type;
	}

	@XmlAttribute(required = true)
	public void setType(AssociationType type) {
		this.type = type;
	}
	
	@Override
	public boolean isRequired() {
		return required;
	}
	
	@XmlTransient
	public void setRequired(boolean required) {
		this.required = required;
	}
	
	@XmlAttribute(name = "required", required = false)
	public void setRequiredBool(Boolean required) {
		this.required = required.booleanValue();
	}
}
