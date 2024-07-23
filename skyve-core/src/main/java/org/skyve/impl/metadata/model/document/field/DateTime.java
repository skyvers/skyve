package org.skyve.impl.metadata.model.document.field;

import org.skyve.impl.metadata.model.document.field.validator.DateValidator;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class DateTime extends ConvertableField {
	private static final long serialVersionUID = 5225280371209879680L;

	private DateValidator validator;

	public DateTime() {
		setAttributeType(AttributeType.dateTime);
	}
	
	public DateValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(DateValidator validator) {
		this.validator = validator;
	}
}
