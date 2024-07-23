package org.skyve.impl.metadata.model.document;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(name = "inverseOne", namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
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
