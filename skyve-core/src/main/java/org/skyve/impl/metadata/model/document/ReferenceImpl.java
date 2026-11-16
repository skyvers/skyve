package org.skyve.impl.metadata.model.document;

import org.skyve.impl.metadata.model.AbstractAttribute;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Abstract base for all reference (relation) attribute implementations.
 *
 * <p>A reference attribute points from one document to another.  This class
 * holds the target {@code documentName}, the target {@code moduleName} (optional,
 * defaults to the owning module), the cascade delete setting, and ownership
 * semantics.  Concrete subclasses specialise as either an
 * {@link AssociationImpl} (many-to-one) or {@link CollectionImpl} (one-to-many).
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see AssociationImpl
 * @see CollectionImpl
 * @see AbstractAttribute
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE,
			propOrder = {"persistentBool", "domainType", "documentName", "queryName"})
public abstract class ReferenceImpl extends AbstractAttribute implements Reference {
	private static final long serialVersionUID = 5426073638143148421L;

	private boolean persistent = true;
	private String documentName;
	private String queryName;

	@Override
	public @Nullable DomainType getDomainType() {
		return domainType;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "domain", required = false)
	public void setDomainType(@Nullable DomainType domainType) {
		this.domainType = domainType;
	}

	@Override
	public @Nonnull String getDocumentName() {
		return documentName;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = true)
	public void setDocumentName(@Nonnull String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	@Override
	public @Nullable String getQueryName() {
		return queryName;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setQueryName(@Nullable String queryName) {
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

	public @Nonnull Boolean getPersistentBool() {
		return Boolean.valueOf(persistent);
	}

	@XmlAttribute(name="persistent", required = false)
	public void setPersistentBool(@Nonnull Boolean persistent) {
		this.persistent = persistent.booleanValue();
	}
}
