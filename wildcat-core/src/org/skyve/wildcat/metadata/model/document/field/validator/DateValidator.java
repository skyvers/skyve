package org.skyve.wildcat.metadata.model.document.field.validator;

import java.util.Date;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.ValidationMessage;
import org.skyve.domain.types.converters.Converter;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class DateValidator extends RangeValidator<Date> {
	@Override
	public void validate(Date value,
							String binding,
							String displayName,
							Converter<Date> converter,
							ValidationException e) {
		if (value != null) {
			Date min = getMin();
			Date max = getMax();
			if (((min != null) && value.before(min)) ||
					((max != null) && value.after(max))) {
				e.getSubordinates().add(new ValidationMessage(binding, constructMessage(displayName, converter)));
			}
		}
	}
}
