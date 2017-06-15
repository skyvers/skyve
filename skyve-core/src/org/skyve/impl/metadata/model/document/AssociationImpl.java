package org.skyve.impl.metadata.model.document;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.Association;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "association")
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, 
			name = "association", 
			propOrder = {"requiredBool", "type", "databaseIndex", "allowCascadeMerge"})
public class AssociationImpl extends ReferenceImpl implements Association {
	private static final long serialVersionUID = -2839713495173145591L;

	private boolean required;
	private AssociationType type;
	private Boolean databaseIndex;
	private Boolean allowCascadeMerge;

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
	
	@Override
	public Boolean getDatabaseIndex() {
		return databaseIndex;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setDatabaseIndex(Boolean databaseIndex) {
		this.databaseIndex = databaseIndex;
	}

	@Override
	public Boolean getAllowCascadeMerge() {
		return allowCascadeMerge;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setAllowCascadeMerge(Boolean allowCascadeMerge) {
		this.allowCascadeMerge = allowCascadeMerge;
	}
}
