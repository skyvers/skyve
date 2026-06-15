package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.DomainType;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * A {@link Field} that may carry an ordered list of validator constraints.
 *
 * <p>Adds a {@code validators} list to {@link Field} to hold
 * {@link org.skyve.impl.metadata.model.document.field.validator} instances
 * (range, regex, length, etc.).  Validators are evaluated in order during domain
 * validation before the bean is saved.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see Field
 * @see ConvertibleField
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class ConstrainableField extends Field {
	private static final long serialVersionUID = 8797830818574132688L;

	@Override
	public DomainType getDomainType() {
		return domainType;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "domain")
	public void setDomainType(DomainType domainType) {
		this.domainType = domainType;
	}
}
