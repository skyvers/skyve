package org.skyve.wildcat.metadata.model.document.field;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.model.document.DomainType;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class Geometry extends Field {

	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -6419883664269071344L;

	public Geometry() {
		setAttributeType(AttributeType.geometry);
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
