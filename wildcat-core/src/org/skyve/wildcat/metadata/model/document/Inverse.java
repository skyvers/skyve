package org.skyve.wildcat.metadata.model.document;

import javax.xml.bind.annotation.XmlAttribute;
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
			propOrder = {"cardinality", "domainType", "documentName", "referenceName"})
public class Inverse extends AbstractAttribute implements Relation {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -2180342709365556857L;

	@XmlTransient
	public static enum InverseRelationship {
		oneToOne,
		oneToMany,
		manyToMany
	}
	
	@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
	public static enum InverseCardinality {
		one, many;
	}

	private String documentName;
	private String referenceName;
	/**
	 * This is only used during domain generation - tests at runtime use the cardinality attribute.
	 */
	private InverseRelationship relationship;
	private InverseCardinality cardinality;
	
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

	/**
	 * This is only used during domain generation - tests at runtime use the cardinality attribute.
	 */
	public InverseRelationship getRelationship() {
		return relationship;
	}

	/**
	 * This is only used during domain generation - tests at runtime use the cardinality attribute.
	 */
	@XmlTransient
	public void setRelationship(InverseRelationship relationship) {
		this.relationship = relationship;
	}

	public InverseCardinality getCardinality() {
		return cardinality;
	}

	@XmlAttribute
	public void setCardinality(InverseCardinality cardinality) {
		this.cardinality = cardinality;
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
