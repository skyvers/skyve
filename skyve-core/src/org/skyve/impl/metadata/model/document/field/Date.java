package org.skyve.impl.metadata.model.document.field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.model.document.field.validator.DateValidator;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.metadata.model.document.field.ConvertableField;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Date extends ConvertableField {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 865730130644671979L;

	private DateValidator validator;
	
	public Date() {
		setAttributeType(AttributeType.date);
	}

	public DateValidator getValidator() {
		return validator;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setValidator(DateValidator validator) {
		this.validator = validator;
	}
}
