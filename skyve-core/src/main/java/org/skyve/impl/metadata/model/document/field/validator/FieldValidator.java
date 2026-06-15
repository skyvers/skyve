package org.skyve.impl.metadata.model.document.field.validator;

import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Validator;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Abstract JAXB base for all field-level validator descriptors in a document XML
 * file.
 *
 * <p>Carries the violation message and an optional condition name that activates
 * the validator only when the named condition evaluates to {@code true}.
 * Concrete subclasses ({@link RangeValidator}, {@link TextValidator}) add
 * type-specific constraint properties.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @param <T> the field value type being validated
 * @see RangeValidator
 * @see TextValidator
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public abstract class FieldValidator<T extends Object> extends Validator<T> implements SerializableMetaData {
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
