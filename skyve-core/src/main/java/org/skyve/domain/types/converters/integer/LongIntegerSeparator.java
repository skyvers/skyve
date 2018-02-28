package org.skyve.domain.types.converters.integer;

import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class LongIntegerSeparator implements Converter<Long> {
	
	@Override
	public String toDisplayValue(Long value) throws Exception {
		return String.format("%,d",value);
	}

	@Override
	public Long fromDisplayValue(String displayValue) throws Exception {
		String result = displayValue.replace(",", "");
		return Long.valueOf(result);
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.longInteger;
	}

	@Override
	public Format<Long> getFormat() {
		return null;
	}

	@Override
	public Validator<Long> getValidator() {
		return null;
	}
}
