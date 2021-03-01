package org.skyve.domain.types.converters.datetime;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Locale;

import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.converters.Converter;

public abstract class AbstractDateTimeConverter implements Converter<DateTime> {

	/**
	 * The pattern for this DateTime converter
	 * 
	 * @return Date time format String pattern
	 */
	public abstract String getPattern();

	@Override
	public DateTime fromDisplayValue(String displayValue) throws Exception {
		DateTimeFormatter formatter = DateTimeFormatter.ofPattern(getPattern(), Locale.ENGLISH);
		LocalDateTime localDateTime = LocalDateTime.parse(displayValue, formatter);
		return new DateTime(Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant()));
	}

	@Override
	public String toDisplayValue(DateTime value) throws Exception {
		DateTimeFormatter formatter = DateTimeFormatter.ofPattern(getPattern(), Locale.ENGLISH);
		LocalDateTime localTime = Instant.ofEpochMilli(value.getTime()).atZone(ZoneId.systemDefault()).toLocalDateTime();
		return formatter.format(localTime);
	}
}
