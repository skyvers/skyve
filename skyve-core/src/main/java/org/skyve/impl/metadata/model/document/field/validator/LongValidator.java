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
public class LongValidator extends RangeValidator<Long> {
	private static final long serialVersionUID = -6691762353103645505L;

	@Override
	public void validate(User user,
							Long value,
							String binding,
							String localisedDisplayName,
							Converter<Long> converter,
							ValidationException e) {
		if (value != null) {
			Long min = getMin();
			Long max = getMax();
			if (((min != null) && (value.compareTo(min) < 0)) ||
					((max != null) && (value.compareTo(max) > 0))) {
				e.getMessages().add(new Message(binding, constructMessage(user, localisedDisplayName, converter)));
			}
		}
	}
}
