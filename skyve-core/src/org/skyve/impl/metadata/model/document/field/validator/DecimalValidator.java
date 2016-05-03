package org.skyve.impl.metadata.model.document.field.validator;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class DecimalValidator extends RangeValidator<Decimal> {
	private Integer precision;

	public Integer getPrecision() {
		return precision;
	}

	@XmlAttribute
	public void setPrecision(Integer precision) {
		this.precision = precision;
	}

	@Override
	public void validate(Decimal value,
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
				e.getMessages().add(new Message(binding, constructMessage(displayName, converter)));
				addedCustomerValidationMessage = (getValidationMessage() != null);
			}
			if ((! addedCustomerValidationMessage) && (precision != null)) {
				int precisionInt = precision.intValue();
				int scale = value.scale();
				if (precisionInt != scale) {
					String message = getValidationMessage();
					if (message == null) {
						e.getMessages().add(new Message(binding, constructPrecisionMessage(displayName)));
					}
				}
			}
		}
	}
	
	public final String constructPrecisionMessage(String displayName) {
		String message = getValidationMessage();
		return (message != null) ? message : (displayName + " does not have the right number of decimal places.");
	}
}
