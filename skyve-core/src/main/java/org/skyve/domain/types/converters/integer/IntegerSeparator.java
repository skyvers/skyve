package org.skyve.domain.types.converters.integer;

import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class IntegerSeparator implements Converter<Integer> {
	
	@Override
	public String toDisplayValue(Integer value) throws Exception {
		return String.format("%,d",value);
	}

	@Override
	public Integer fromDisplayValue(String displayValue) throws Exception {
		String result = displayValue.replace(",", "");
		return Integer.valueOf(Integer.parseInt(result));
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.integer;
	}

	@Override
	public Format<Integer> getFormat() {
		return null;
	}

	@Override
	public Validator<Integer> getValidator() {
		return null;
	}
}
