package org.skyve.impl.metadata.model.document;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

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
