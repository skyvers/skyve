package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Image extends Content {
	private static final long serialVersionUID = 1207840577273060855L;

	public Image() {
		setAttributeType(AttributeType.image);
	}
}
