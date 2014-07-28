package org.skyve.wildcat.metadata.model.document.field.validator;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.ValidationMessage;
import org.skyve.domain.types.converters.Converter;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.DOCUMENT_NAMESPACE)
public class IntegerValidator extends RangeValidator<Integer> {
	@Override
	public void validate(Integer value,
							String binding,
							String displayName,
							Converter<Integer> converter,
							ValidationException e) {
		if (value != null) {
			Integer min = getMin();
			Integer max = getMax();
			if (((min != null) && (value.compareTo(min) < 0)) ||
					((max != null) && (value.compareTo(max) > 0))) {
				e.getSubordinates().add(new ValidationMessage(binding, constructMessage(displayName, converter)));
			}
		}
	}
}
