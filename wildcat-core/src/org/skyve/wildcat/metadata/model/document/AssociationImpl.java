package org.skyve.wildcat.metadata.model.document;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.model.document.Association;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE, name = "association")
@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE, 
			name = "association", 
			propOrder = {"requiredBool", "type"})
public class AssociationImpl extends Reference implements Association {
	private static final long serialVersionUID = -2839713495173145591L;

	private boolean required;
	private AssociationType type;

	public AssociationImpl() {
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
