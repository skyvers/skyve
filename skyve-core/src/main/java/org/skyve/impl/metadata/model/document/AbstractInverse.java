package org.skyve.impl.metadata.model.document;

import org.skyve.impl.metadata.model.AbstractAttribute;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Inverse;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE,
			propOrder = {"domainType", "documentName", "referenceName"})
/**
 * Abstract base for inverse navigation attributes.
 *
 * <p>An inverse attribute provides a navigable back-reference from the target
 * document to the source document of a collection or association.  Unlike a
 * forward reference, an inverse attribute carries no persistence column; it is
 * resolved at runtime via the owning relation.  Concrete subclasses differentiate
 * between a single inverse reference ({@link InverseOne}) and a collection
 * inverse ({@link InverseMany}).
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see InverseOne
 * @see InverseMany
 * @see AbstractAttribute
 */
public abstract class AbstractInverse extends AbstractAttribute implements Inverse {
	private static final long serialVersionUID = 6617399816835649143L;

	@XmlTransient
	@SuppressWarnings("java:S115") // Enum names are metadata generation values.
	public enum InverseRelationship {
		oneToOne,
		oneToMany,
		manyToMany
	}
	
	private String documentName;
	private String referenceName;

	/**
	 * This is only used during domain generation - tests at runtime use the cardinality attribute.
	 */
	private InverseRelationship relationship;
	private Boolean cascade;

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
	public String getReferenceName() {
		return referenceName;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = true)
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

	@Override
	public Boolean getCascade() {
		return cascade;
	}

	/**
	 * If this inverse is cascading, trackChanges will be used, unless it's explicitly set off
	 */
	@XmlAttribute
	public void setCascade(Boolean cascade) {
		this.cascade = cascade;
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
