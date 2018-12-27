package org.skyve.impl.metadata.model.document.field.validator;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.user.User;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class IntegerValidator extends RangeValidator<Integer> {
	private static final long serialVersionUID = -4605898071552499592L;

	@Override
	public void validate(User user,
							Integer value,
							String binding,
							String displayName,
							Converter<Integer> converter,
							ValidationException e) {
		if (value != null) {
			Integer min = getMin();
			Integer max = getMax();
			if (((min != null) && (value.compareTo(min) < 0)) ||
					((max != null) && (value.compareTo(max) > 0))) {
				e.getMessages().add(new Message(binding, constructMessage(user, displayName, converter)));
			}
		}
	}
}
