package org.skyve.impl.metadata.model.document.field;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.metadata.model.document.field.ConvertableField;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Boolean extends ConvertableField {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 4887475952064140008L;

	public Boolean() {
		setAttributeType(AttributeType.bool);
	}
}
