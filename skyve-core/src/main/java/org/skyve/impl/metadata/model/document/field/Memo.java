package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Memo extends ConstrainableField {
	private static final long serialVersionUID = 6729451150523368478L;

	public Memo() {
		setAttributeType(AttributeType.memo);
	}
}
