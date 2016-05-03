package org.skyve.impl.metadata.model.document.field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLUtil;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.impl.metadata.model.document.field.Field;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class ConstrainableField extends Field {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 8797830818574132688L;

	@Override
	public DomainType getDomainType() {
		return domainType;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE, name = "domain")
	public void setDomainType(DomainType domainType) {
		this.domainType = domainType;
	}
}
