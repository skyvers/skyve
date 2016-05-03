package org.skyve.impl.metadata.model.document.field;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLUtil;
import org.skyve.impl.metadata.model.document.field.ConstrainableField;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class Memo extends ConstrainableField {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 6729451150523368478L;

	public Memo() {
		setAttributeType(AttributeType.memo);
	}
}
