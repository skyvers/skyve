package org.skyve.domain.types.converters.timestamp;

import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.domain.types.converters.datetime.AbstractTimestampConverter;
import org.skyve.metadata.model.Attribute.AttributeType;

public class MMM_DD_YYYY_HH_MI_SS extends AbstractTimestampConverter implements Converter<Timestamp> {

	public static final String PATTERN = "MMM-dd-yyyy hh:mm:ss a";

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.timestamp;
	}

	@Override
	public Format<Timestamp> getFormat() {
		return null;
	}

	@Override
	public Validator<Timestamp> getValidator() {
		return null;
	}

	@Override
	public String getPattern() {
		return PATTERN;
	}
}
