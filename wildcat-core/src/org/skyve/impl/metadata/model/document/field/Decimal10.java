package org.skyve.impl.metadata.model.document.field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.util.XMLUtil;
import org.skyve.impl.metadata.model.document.field.ConvertableField;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class Decimal10 extends ConvertableField {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 7235405478552339257L;

	private DecimalValidator validator;
	
	public Decimal10() {
		setAttributeType(AttributeType.decimal10);
	}
	
	public DecimalValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
	public void setValidator(DecimalValidator validator) {
		this.validator = validator;
	}
}
