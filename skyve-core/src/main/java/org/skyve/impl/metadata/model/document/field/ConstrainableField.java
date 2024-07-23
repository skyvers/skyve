package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.DomainType;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

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
