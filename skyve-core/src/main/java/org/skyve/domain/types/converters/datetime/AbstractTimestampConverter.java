package org.skyve.domain.types.converters.datetime;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Locale;

import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;

public abstract class AbstractTimestampConverter implements Converter<Timestamp> {

	/**
	 * The pattern for this Timestamp converter
	 * 
	 * @return Timestamp format String pattern
	 */
	public abstract String getPattern();

	@Override
	public Timestamp fromDisplayValue(String displayValue) throws Exception {
		DateTimeFormatter formatter = DateTimeFormatter.ofPattern(getPattern(), Locale.ENGLISH);
		LocalDateTime localDateTime = LocalDateTime.parse(displayValue, formatter);
		return new Timestamp(Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant()));
	}

	@Override
	public String toDisplayValue(Timestamp value) throws Exception {
		DateTimeFormatter formatter = DateTimeFormatter.ofPattern(getPattern(), Locale.ENGLISH);
		LocalDateTime localTime = Instant.ofEpochMilli(value.getTime()).atZone(ZoneId.systemDefault()).toLocalDateTime();
		return formatter.format(localTime);
	}
}
