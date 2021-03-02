package org.skyve.domain.types.converters.datetime;

import org.skyve.CORE;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public abstract class AbstractDateTimeConverter implements Converter<DateTime> {
	@Override
	public final AttributeType getAttributeType() {
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

	/**
	 * The pattern for this DateTime converter
	 * 
	 * @return Date time format String pattern
	 */
	protected abstract String getPattern();

	@Override
	public DateTime fromDisplayValue(String displayValue) throws Exception {
		// DateTimeFormatter formatter = DateTimeFormatter.ofPattern(getPattern(), Locale.ENGLISH);
		// LocalDateTime localDateTime = LocalDateTime.parse(displayValue, formatter);
		// return new DateTime(Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant()));
		return new DateTime(CORE.getDateFormat(getPattern()).parse(displayValue).getTime());
	}

	@Override
	public String toDisplayValue(DateTime value) throws Exception {
		// DateTimeFormatter formatter = DateTimeFormatter.ofPattern(getPattern(), Locale.ENGLISH);
		// LocalDateTime localTime = Instant.ofEpochMilli(value.getTime()).atZone(ZoneId.systemDefault()).toLocalDateTime();
		// return formatter.format(localTime);
		return CORE.getDateFormat(getPattern()).format(value);
	}
}
