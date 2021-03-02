package org.skyve.domain.types.converters.time;

import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.domain.types.converters.datetime.AbstractTimeConverter;
import org.skyve.metadata.model.Attribute.AttributeType;

public class HH_MI extends AbstractTimeConverter implements Converter<TimeOnly> {

	private static final String PATTERN = "hh:mm a";

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.time;
	}

	@Override
	public Format<TimeOnly> getFormat() {
		return null;
	}

	@Override
	public Validator<TimeOnly> getValidator() {
		return null;
	}

	@Override
	public String getPattern() {
		return PATTERN;
	}
}
