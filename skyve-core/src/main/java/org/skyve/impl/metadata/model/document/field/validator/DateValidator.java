package org.skyve.impl.metadata.model.document.field.validator;

import java.util.Date;

import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.user.User;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class DateValidator extends RangeValidator<Date> {
	private static final long serialVersionUID = 2052940948388013357L;

	@Override
	public void validate(User user, 
							Date value,
							String binding,
							String localisedDisplayName,
							Converter<Date> converter,
							ValidationException e) {
		if (value != null) {
			Date min = getMin();
			Date max = getMax();
			if (((min != null) && value.before(min)) ||
					((max != null) && value.after(max))) {
				e.getMessages().add(new Message(binding, constructMessage(user, localisedDisplayName, converter)));
			}
		}
	}
}
