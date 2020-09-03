package org.skyve.impl.metadata.model.document.field.validator;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.apache.commons.validator.GenericValidator;
import org.apache.commons.validator.routines.DomainValidator;
import org.apache.commons.validator.routines.ISBNValidator;
import org.apache.commons.validator.routines.InetAddressValidator;
import org.apache.commons.validator.routines.checkdigit.EAN13CheckDigit;
import org.apache.commons.validator.routines.checkdigit.IBANCheckDigit;
import org.apache.commons.validator.routines.checkdigit.ISINCheckDigit;
import org.apache.commons.validator.routines.checkdigit.LuhnCheckDigit;
import org.apache.commons.validator.routines.checkdigit.VerhoeffCheckDigit;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class TextValidator extends FieldValidator<String> {

	private static final long serialVersionUID = -1957608134362029502L;
	/**
	 * Default protocol scheme which will prepended when a URL being validated does not supply one.
	 */
	private static final String DEFAULT_PROTOCOL = "http://";
	private static final String PATTERN_EMAIL = "^[^@]+@[^@]+$";
	private static final String PATTERN_PROTOCOL = "^(?:(ht|f)tp(s?)\\:\\/\\/)+.*$";

	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public static enum ValidatorType {
		creditCard,
		internetDomain,
		ean13CheckDigit,
		email,
		ibanCheckDigit,
		ipAddress,
		ipv4Address,
		isbnCheckDigit,
		isinCheckDigit,
		luhnCheckDigit,
		url,
		verhoeffCheckDigit
	}
	
	private String regularExpression;
	private ValidatorType type;

	public String getRegularExpression() {
		return regularExpression;
	}

	@XmlAttribute
	public void setRegularExpression(String regularExpression) {
		this.regularExpression = UtilImpl.processStringValue(regularExpression);
	}

	public ValidatorType getType() {
		return type;
	}

	@XmlAttribute
	public void setType(ValidatorType type) {
		this.type = type;
		
		// for client-side (if applicable)
		if (regularExpression == null) {
			if (ValidatorType.email.equals(type)) {
				regularExpression = PATTERN_EMAIL;
			}
			else if (ValidatorType.creditCard.equals(type)) {
				regularExpression = "^\\d{16}$";
			}
			else if (ValidatorType.ipv4Address.equals(type)) {
				regularExpression = "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$";
			}
		}
	}
	
	@Override
	public void validate(User user,
							String value,
							String binding,
							String displayName,
							Converter<String> converter,
							ValidationException e) {
		if (value != null) {
			boolean valid = true;
			if (regularExpression != null) {
				valid = GenericValidator.matchRegexp(value, regularExpression);
			}
			
			if (valid && (type != null)) {
				switch (type) {
					case creditCard:
						valid = GenericValidator.isCreditCard(value);
						break;
					case ean13CheckDigit:
						valid = new EAN13CheckDigit().isValid(value);
						break;
					case email:
						// NB The commons email validator doesn't handle registered domain suffixes
						// correctly - ie poo@wee - it expects poo@wee.com
						valid = GenericValidator.matchRegexp(value, PATTERN_EMAIL);
						break;
					case ibanCheckDigit:
						valid = new IBANCheckDigit().isValid(value);
						break;
					case internetDomain:
						valid = DomainValidator.getInstance().isValid(value);
						break;
					case ipAddress:
						valid = InetAddressValidator.getInstance().isValid(value);
						break;
					case ipv4Address:
						valid = InetAddressValidator.getInstance().isValidInet4Address(value);
						break;
					case isbnCheckDigit:
						valid = ISBNValidator.getInstance().isValid(value);
						break;
					case isinCheckDigit:
						valid = new ISINCheckDigit().isValid(value);
						break;
					case luhnCheckDigit:
						valid = new LuhnCheckDigit().isValid(value);
						break;
					case url:
						// if the supplied value does not contain a protocol, prepend one
						if (!value.matches(PATTERN_PROTOCOL)) {
							valid = GenericValidator.isUrl(DEFAULT_PROTOCOL + value);
						} else {
							valid = GenericValidator.isUrl(value);
						}
						break;
					case verhoeffCheckDigit:
						valid = new VerhoeffCheckDigit().isValid(value);
						break;
					default:
						throw new IllegalStateException("TextValidation type " + type + " is not catered for.");
				}
			}
			
			if (! valid) {
				e.getMessages().add(new Message(binding, constructMessage(user, displayName, converter)));
			}
		}
	}

	@Override
	public String constructMessage(User user, String displayName, Converter<String> converter) {
		String message = Util.i18n(getValidationMessage(), user.getLocale());
		return (message != null) ? message : (displayName + " is not formatted correctly.");
	}
}
