package org.skyve.impl.metadata.model.document.field;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLUtil;
import org.skyve.impl.metadata.model.document.field.ConvertableField;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class Colour extends ConvertableField {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2815253897974678949L;

	public Colour() {
		setAttributeType(AttributeType.colour);
	}
}
