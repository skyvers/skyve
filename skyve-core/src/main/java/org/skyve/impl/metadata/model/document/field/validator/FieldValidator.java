package org.skyve.impl.metadata.model.document.field.validator;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Validator;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public abstract class FieldValidator<T extends Object> extends Validator<T> implements MetaData {
	private static final long serialVersionUID = -7074155220758634937L;

	private String validationMessage;
	
	public String getValidationMessage() {
		return validationMessage;
	}
	
	public String getLocalisedValidationMessage() {
		return Util.i18n(validationMessage);
	}
	
	@XmlAttribute
	public void setValidationMessage(String validationMessage) {
		this.validationMessage = UtilImpl.processStringValue(validationMessage);
	}
	
	public abstract String constructMessage(User user, String localisedDisplayName, Converter<T> converter);
}
