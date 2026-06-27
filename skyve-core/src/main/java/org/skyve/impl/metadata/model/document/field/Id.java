package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.DomainType;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated field type representing the {@code bizId} primary-key attribute.
 *
 * <p>Every persistent Skyve document has exactly one {@code Id} field named
 * {@code bizId}.  The value is a UUID string generated at construction time
 * (see {@link org.skyve.impl.util.UUIDv7}).  The column is declared as
 * {@code VARCHAR(36)} in the DDL and carries a unique constraint.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see Field
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Id extends Field {
	private static final long serialVersionUID = -4193089805642262555L;

	public Id() {
		setAttributeType(AttributeType.id);
	}
	
	/**
	 * Not used
	 */
	@Override
	@XmlTransient
	public DomainType getDomainType() {
		return null;
	}
}
