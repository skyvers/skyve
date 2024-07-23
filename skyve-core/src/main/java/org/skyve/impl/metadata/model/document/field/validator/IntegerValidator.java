package org.skyve.impl.metadata.model.document.field.validator;

import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.user.User;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class IntegerValidator extends RangeValidator<Integer> {
	private static final long serialVersionUID = -4605898071552499592L;

	@Override
	public void validate(User user,
							Integer value,
							String binding,
							String localisedDisplayName,
							Converter<Integer> converter,
							ValidationException e) {
		if (value != null) {
			Integer min = getMin();
			Integer max = getMax();
			if (((min != null) && (value.compareTo(min) < 0)) ||
					((max != null) && (value.compareTo(max) > 0))) {
				e.getMessages().add(new Message(binding, constructMessage(user, localisedDisplayName, converter)));
			}
		}
	}
}
