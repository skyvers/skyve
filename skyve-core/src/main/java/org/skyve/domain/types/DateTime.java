package org.skyve.domain.types;

import java.text.ParseException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;

import org.skyve.impl.util.TimeUtil;

/**
 * Date and time (usually without seconds).
 */
public class DateTime extends Date {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -3775522231628914522L;

	/**
	 * 
	 */
	public DateTime() {
		super();
		TimeUtil.clearMillisecondComponent(this);
	}

	/**
	 * 
	 * @param date
	 */
	public DateTime(long date) {
		super(date);
		TimeUtil.clearMillisecondComponent(this);
	}
	
	/**
	 * 
	 * @param date
	 */
	public DateTime(Date date) {
		this(date.getTime());
	}
	
	/**
	 * 
	 * @param serializedForm
	 * @throws ParseException
	 */
	public DateTime(String serializedForm) throws ParseException {
		this(TimeUtil.parseISODate(serializedForm));
	}

	/**
	 * 
	 */
	@Override
	public String toString() {
		return TimeUtil.formatISODate(this, true);
	}
	
	public LocalDate toLocalDate() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
	}

	public LocalDateTime toLocalDateTime() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
	}

	public LocalTime toLocalTime() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalTime();
	}

	public DateTime setLocalDate(LocalDate date) {
		setTime(date.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
		return this; // for EL or method chaining
	}

	public DateTime setLocalDateTime(LocalDateTime date) {
		setTime(date.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
		TimeUtil.clearMillisecondComponent(this);
		return this; // for EL or method chaining
	}
}
