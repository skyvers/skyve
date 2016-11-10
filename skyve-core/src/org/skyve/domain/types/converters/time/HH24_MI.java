package org.skyve.domain.types.converters.time;

import org.skyve.CORE;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class HH24_MI implements Converter<TimeOnly> {
	private static final String PATTERN = "HH:mm";

	@Override
	public TimeOnly fromDisplayValue(String displayValue) throws Exception {
		return new TimeOnly(CORE.getDateFormat(PATTERN).parse(displayValue).getTime());
	}

	@Override
	public String toDisplayValue(TimeOnly value) throws Exception {
		return CORE.getDateFormat(PATTERN).format(value);
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.time;
	}

	@Override
	public Format<TimeOnly> getFormat() {
		return new Format<>("##:##", null);
	}

	@Override
	public Validator<TimeOnly> getValidator() {
		return null;
	}
}
