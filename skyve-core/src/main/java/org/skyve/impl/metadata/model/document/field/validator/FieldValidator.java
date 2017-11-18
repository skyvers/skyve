package org.skyve.impl.metadata.model.document.field.validator;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Validator;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public abstract class FieldValidator<T extends Object> extends Validator<T>  {
	private String validationMessage;
	
	public String getValidationMessage() {
		return validationMessage;
	}
	
	@XmlAttribute
	public void setValidationMessage(String validationMessage) {
		this.validationMessage = UtilImpl.processStringValue(validationMessage);
	}
	
	public abstract String constructMessage(String displayName, Converter<T> converter);
}
