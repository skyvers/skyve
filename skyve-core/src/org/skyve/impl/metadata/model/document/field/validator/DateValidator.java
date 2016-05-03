package org.skyve.impl.metadata.model.document.field.validator;

import java.util.Date;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.util.XMLUtil;

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
				e.getMessages().add(new Message(binding, constructMessage(displayName, converter)));
			}
		}
	}
}
