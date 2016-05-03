package org.skyve.impl.metadata.model.document.field;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLUtil;
import org.skyve.impl.metadata.model.document.field.Memo;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class Markup extends Memo {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4849631120596097936L;

	public Markup() {
		setAttributeType(AttributeType.markup);
	}
}
