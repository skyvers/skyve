package org.skyve.impl.metadata.model.document;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

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
