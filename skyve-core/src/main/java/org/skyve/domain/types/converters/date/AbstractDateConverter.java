package org.skyve.domain.types.converters.date;

import org.skyve.CORE;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public abstract class AbstractDateConverter implements Converter<DateOnly> {
	@Override
	public final AttributeType getAttributeType() {
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

	/**
	 * The pattern for this Date converter
	 * 
	 * @return Date time format String pattern
	 */
	protected abstract String getPattern();

	@Override
	public DateOnly fromDisplayValue(String displayValue) throws Exception {
		return new DateOnly(CORE.getDateFormat(getPattern()).parse(displayValue).getTime());
	}

	@Override
	public String toDisplayValue(DateOnly value) throws Exception {
		return CORE.getDateFormat(getPattern()).format(value);
	}
}
