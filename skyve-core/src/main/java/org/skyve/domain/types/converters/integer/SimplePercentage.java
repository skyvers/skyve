package org.skyve.domain.types.converters.integer;

import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class SimplePercentage implements Converter<Integer> {
	@Override
	public String toDisplayValue(Integer value) throws Exception {
		return value + "%";
	}

	@Override
	public Integer fromDisplayValue(String displayValue) throws Exception {
		displayValue.replace("%", "");
		return new Integer(displayValue);
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
