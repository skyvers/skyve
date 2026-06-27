package org.skyve.impl.metadata.model.document;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.OrderingImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.Ordering;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Inverse navigation attribute that resolves to a collection of beans.
 *
 * <p>Represents the one-side of a one-to-many relation, allowing the parent
 * document to navigate back to all associated child records without declaring
 * an explicit collection.  Implements {@link OrderedAttribute} to support
 * explicit element ordering.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see AbstractInverse
 * @see InverseOne
 * @see OrderedAttribute
 */
@XmlRootElement(name = "inverseMany", namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, propOrder = {"ordering"})
public class InverseMany extends AbstractInverse implements OrderedAttribute {
	private static final long serialVersionUID = -8376859153723226264L;

	/**
	 * The ordering of this collection
	 */
	private List<Ordering> ordering = new ArrayList<>();

	/**
	 * Populated by Document.convert(), this indicates that the order by statement contains at least 1 compound binding.
	 * This means that the framework must sort this collection once it is loaded, not in the SQL statement.
	 */
	private boolean complexOrdering = false;

	public InverseMany() {
		setAttributeType(AttributeType.inverseMany);
	}

	@Override
	public InverseCardinality getCardinality() {
		return InverseCardinality.many;
	}
	
	@Override
	@XmlElementWrapper(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "ordering")
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "order", type = OrderingImpl.class, required = false)
	public List<Ordering> getOrdering() {
		return ordering;
	}
	
	@Override
	public boolean isComplexOrdering() {
		return complexOrdering;
	}

	@XmlTransient
	public void setComplexOrdering(boolean complexOrdering) {
		this.complexOrdering = complexOrdering;
	}
}
