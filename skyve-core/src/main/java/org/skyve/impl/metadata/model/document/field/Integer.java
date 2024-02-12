package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Integer extends ConvertableField {
	private static final long serialVersionUID = 884934642737849696L;

	private IntegerValidator validator;
	
	public Integer() {
		setAttributeType(AttributeType.integer);
	}
	
	public IntegerValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(IntegerValidator validator) {
		this.validator = validator;
	}
}
