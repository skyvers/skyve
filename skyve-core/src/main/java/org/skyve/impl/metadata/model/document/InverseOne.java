package org.skyve.impl.metadata.model.document;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Inverse navigation attribute that resolves to a single bean.
 *
 * <p>Represents the many-side of a one-to-many relation, allowing the child
 * document to navigate back to its parent.  At runtime, resolution follows the
 * owning collection or association and returns a single {@link org.skyve.domain.Bean}
 * reference (or {@code null} if no parent exists).
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see AbstractInverse
 * @see InverseMany
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(name = "inverseOne", namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class InverseOne extends AbstractInverse {
	private static final long serialVersionUID = -2180342709365556857L;

	public InverseOne() {
		setAttributeType(AttributeType.inverseOne);
	}

	@Override
	public InverseCardinality getCardinality() {
		return InverseCardinality.one;
	}
}
