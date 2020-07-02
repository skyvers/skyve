package org.skyve.domain.types.converters.datetime;

import org.skyve.CORE;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class DD_MM_YYYY_HH_MI implements Converter<DateTime> {

	public static final String PATTERN = "dd/MM/yyyy hh:mm a";

	@Override
	public DateTime fromDisplayValue(String displayValue) throws Exception {
		return new DateTime(CORE.getDateFormat(PATTERN).parse(displayValue).getTime());
	}

	@Override
	public String toDisplayValue(DateTime value) throws Exception {
		return CORE.getDateFormat(PATTERN).format(value);
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.dateTime;
	}

	@Override
	public Format<DateTime> getFormat() {
		return null;
	}

	@Override
	public Validator<DateTime> getValidator() {
		return null;
	}
}
