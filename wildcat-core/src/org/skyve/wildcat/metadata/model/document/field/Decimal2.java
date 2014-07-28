package org.skyve.wildcat.metadata.model.document.field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class Decimal2 extends ConvertableField {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4736160451221243314L;

	DecimalValidator validator;
	
	public Decimal2() {
		setAttributeType(AttributeType.decimal2);
	}
	
	public DecimalValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
	public void setValidator(DecimalValidator validator) {
		this.validator = validator;
	}
}
