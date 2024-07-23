package org.skyve.impl.metadata.model.document;

import org.skyve.impl.metadata.model.AbstractAttribute;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE,
			propOrder = {"persistentBool", "domainType", "documentName", "queryName"})
public abstract class ReferenceImpl extends AbstractAttribute implements Reference {
	private static final long serialVersionUID = 5426073638143148421L;

	private boolean persistent = true;
	private String documentName;
	private String queryName;

	@Override
	public DomainType getDomainType() {
		return domainType;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "domain", required = false)
	public void setDomainType(DomainType domainType) {
		this.domainType = domainType;
	}

	@Override
	public String getDocumentName() {
		return documentName;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	@Override
	public String getQueryName() {
		return queryName;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}
	
	@Override
	public boolean isPersistent() {
		return persistent;
	}

	@XmlTransient
	public void setPersistent(boolean persistent) {
		this.persistent = persistent;
	}

	public Boolean getPersistentBool() {
		return Boolean.valueOf(persistent);
	}

	@XmlAttribute(name="persistent", required = false)
	public void setPersistentBool(Boolean persistent) {
		this.persistent = persistent.booleanValue();
	}
}
