package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Decimal10 extends ConvertableField {
	private static final long serialVersionUID = 7235405478552339257L;

	private DecimalValidator validator;
	
	public Decimal10() {
		setAttributeType(AttributeType.decimal10);
	}
	
	public DecimalValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(DecimalValidator validator) {
		this.validator = validator;
	}
}
