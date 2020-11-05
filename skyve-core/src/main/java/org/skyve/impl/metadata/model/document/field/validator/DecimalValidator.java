package org.skyve.impl.metadata.model.document.field.validator;

import java.util.Locale;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.user.User;
import org.skyve.util.BeanValidator;
import org.skyve.util.Util;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class DecimalValidator extends RangeValidator<Decimal> {
	private static final long serialVersionUID = -8883741869433458702L;

	private Integer precision;

	public Integer getPrecision() {
		return precision;
	}

	@XmlAttribute
	public void setPrecision(Integer precision) {
		this.precision = precision;
	}

	@Override
	public void validate(User user,
							Decimal value,
							String binding,
							String displayName,
							Converter<Decimal> converter,
							ValidationException e) {
		if (value != null) {
			boolean addedCustomerValidationMessage = false;
			Decimal min = getMin();
			Decimal max = getMax();
			if (((min != null) && (value.compareTo(min) < 0)) ||
					((max != null) && (value.compareTo(max) > 0))) {
				e.getMessages().add(new Message(binding, constructMessage(user, displayName, converter)));
				addedCustomerValidationMessage = (getValidationMessage() != null);
			}
			if ((! addedCustomerValidationMessage) && (precision != null)) {
				int precisionInt = precision.intValue();
				int scale = value.scale();
				if (precisionInt != scale) {
					String message = getValidationMessage();
					if (message == null) {
						e.getMessages().add(new Message(binding, constructPrecisionMessage(displayName, user.getLocale())));
					}
				}
			}
		}
	}
	
	public final String constructPrecisionMessage(String displayName, Locale locale) {
		String result = Util.i18n(getValidationMessage(), locale);
		if (result == null) {
			result = Util.i18n(BeanValidator.VALIDATION_PRECISION_KEY, locale, Util.i18n(displayName, locale), precision.toString());
		}
		return result;
	}
}
