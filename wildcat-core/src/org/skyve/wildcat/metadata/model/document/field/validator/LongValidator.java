package org.skyve.wildcat.metadata.model.document.field.validator;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.types.converters.Converter;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class LongValidator extends RangeValidator<Long> {
	@Override
	public void validate(Long value,
							String binding,
							String displayName,
							Converter<Long> converter,
							ValidationException e) {
		if (value != null) {
			Long min = getMin();
			Long max = getMax();
			if (((min != null) && (value.compareTo(min) < 0)) ||
					((max != null) && (value.compareTo(max) > 0))) {
				e.getMessages().add(new Message(binding, constructMessage(displayName, converter)));
			}
		}
	}
}
