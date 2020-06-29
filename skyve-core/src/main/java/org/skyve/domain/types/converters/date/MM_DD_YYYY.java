package org.skyve.domain.types.converters.date;

import org.skyve.CORE;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class MM_DD_YYYY implements Converter<DateOnly> {

	public static final String PATTERN = "MM/dd/yyyy";

	@Override
	public DateOnly fromDisplayValue(String displayValue) throws Exception {
		return new DateOnly(CORE.getDateFormat(PATTERN).parse(displayValue).getTime());
	}

	@Override
	public String toDisplayValue(DateOnly value) throws Exception {
		return CORE.getDateFormat(PATTERN).format(value);
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.date;
	}

	@Override
	public Format<DateOnly> getFormat() {
		return null;
	}

	@Override
	public Validator<DateOnly> getValidator() {
		return null;
	}
}
