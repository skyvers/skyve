package org.skyve.impl.metadata.model.document;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.Association;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "association")
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, 
			name = "association", 
			propOrder = {"requiredBool", "type", "embeddedColumnsPrefix", "databaseIndex"})
public class AssociationImpl extends ReferenceImpl implements Association {
	private static final long serialVersionUID = -2839713495173145591L;

	private boolean required;
	private AssociationType type;
	private String embeddedColumnsPrefix;
	private Boolean databaseIndex;

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
	
	public Boolean getRequiredBool() {
		return Boolean.valueOf(required);
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
	public String getEmbeddedColumnsPrefix() {
		return embeddedColumnsPrefix;
	}

	@XmlAttribute(required = false)
	public void setEmbeddedColumnsPrefix(String embeddedColumnsPrefix) {
		this.embeddedColumnsPrefix = embeddedColumnsPrefix;
	}
	
	@Override
	public Boolean getDatabaseIndex() {
		return databaseIndex;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setDatabaseIndex(Boolean databaseIndex) {
		this.databaseIndex = databaseIndex;
	}
}
