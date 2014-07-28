package org.skyve.wildcat.metadata.model.document;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Relation;
import org.skyve.wildcat.metadata.model.AbstractAttribute;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE,
			propOrder = {"domainType", "documentName", "referenceName"})
public class Inverse extends AbstractAttribute implements Relation {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -2180342709365556857L;

	@XmlTransient
	public static enum InverseRelationship {
		oneToMany,
		manyToMany
	}
	
	private String documentName;
	private String referenceName;
	private InverseRelationship relationship;
	
	public Inverse() {
		setAttributeType(AttributeType.inverse);
	}

	@Override
	public DomainType getDomainType() {
		return domainType;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE, name = "domain", required = false)
	public void setDomainType(DomainType domainType) {
		this.domainType = domainType;
	}

	@Override
	public String getDocumentName() {
		return documentName;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE, required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public String getReferenceName() {
		return referenceName;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE, required = true)
	public void setReferenceName(String referenceName) {
		this.referenceName = UtilImpl.processStringValue(referenceName);
	}

	public InverseRelationship getRelationship() {
		return relationship;
	}

	@XmlTransient
	public void setRelationship(InverseRelationship relationship) {
		this.relationship = relationship;
	}

	@Override
	public boolean isRequired() {
		return false;
	}

	@Override
	public boolean isPersistent() {
		return true;
	}
}
