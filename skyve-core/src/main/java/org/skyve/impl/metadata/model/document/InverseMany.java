package org.skyve.impl.metadata.model.document;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(name = "inverseMany", namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class InverseMany extends AbstractInverse {
	private static final long serialVersionUID = -8376859153723226264L;

	public InverseMany() {
		setAttributeType(AttributeType.inverseMany);
	}

	@Override
	public InverseCardinality getCardinality() {
		return InverseCardinality.many;
	}
}
