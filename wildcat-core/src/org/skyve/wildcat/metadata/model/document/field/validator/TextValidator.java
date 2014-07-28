package org.skyve.wildcat.metadata.model.document.field.validator;

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
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.ValidationMessage;
import org.skyve.domain.types.converters.Converter;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class TextValidator extends FieldValidator<String> {
	@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
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
	public String getRegularExpression() {
		return regularExpression;
	}
	@XmlAttribute
	public void setRegularExpression(String regularExpression) {
		this.regularExpression = UtilImpl.processStringValue(regularExpression);
	}

	private ValidatorType type;
	public ValidatorType getType() {
		return type;
	}
	@XmlAttribute
	public void setType(ValidatorType type) {
		this.type = type;
		
		// for client-side (if applicable)
		if (regularExpression == null) {
			if (ValidatorType.email.equals(type)) {
				regularExpression = "^([a-zA-Z0-9_\\.\\-])+@([a-zA-Z0-9_\\.\\-])+$";
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
	public void validate(String value,
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
					valid = GenericValidator.isEmail(value);
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
					valid = GenericValidator.isUrl(value);
					break;
				case verhoeffCheckDigit:
					valid = new VerhoeffCheckDigit().isValid(value);
					break;
				default:
					throw new IllegalStateException("TextValidation type " + type + " is not catered for.");
				}
			}
			
			if (! valid) {
				e.getSubordinates().add(new ValidationMessage(binding, constructMessage(displayName, converter)));
			}
		}
	}

	@Override
	public String constructMessage(String displayName, Converter<String> converter) {
		String message = getValidationMessage();
		return (message != null) ? message : (displayName + " is not formatted correctly.");
	}
}
