package org.skyve.domain.types.converters.datetime;

import org.skyve.CORE;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.converters.Converter;

public abstract class AbstractTimeConverter implements Converter<TimeOnly> {

	/**
	 * The pattern for this TimeOnly converter
	 * 
	 * @return Time format String pattern
	 */
	public abstract String getPattern();

	@Override
	public TimeOnly fromDisplayValue(String displayValue) throws Exception {
		/*DateTimeFormatter formatter = DateTimeFormatter.ofPattern(getPattern(), Locale.ENGLISH);
		LocalTime localTime = LocalTime.parse(displayValue, formatter);
		
		Instant timeOnEpochDayInDefaultTimeZone = LocalDate.EPOCH
		        .atTime(localTime)
		        .atZone(ZoneId.systemDefault())
		        .toInstant();
		
		return new TimeOnly(Date.from(timeOnEpochDayInDefaultTimeZone));*/
		return new TimeOnly(CORE.getDateFormat(getPattern()).parse(displayValue).getTime());
	}

	@Override
	public String toDisplayValue(TimeOnly value) throws Exception {
		/*DateTimeFormatter formatter = DateTimeFormatter.ofPattern(getPattern(), Locale.ENGLISH);
		LocalDateTime localTime = Instant.ofEpochMilli(value.getTime()).atZone(ZoneId.systemDefault()).toLocalDateTime();
		return formatter.format(localTime);*/
		return CORE.getDateFormat(getPattern()).format(value);
	}
}
