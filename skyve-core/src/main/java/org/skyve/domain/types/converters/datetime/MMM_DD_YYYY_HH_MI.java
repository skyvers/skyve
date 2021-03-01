package org.skyve.domain.types.converters.datetime;

import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class MMM_DD_YYYY_HH_MI extends AbstractDateTimeConverter {

	public static final String PATTERN = "MMM-dd-yyyy hh:mm a";


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

	@Override
	public String getPattern() {
		return PATTERN;
	}
}
