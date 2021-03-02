package org.skyve.domain.types;

import java.text.ParseException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;

import org.skyve.impl.util.TimeUtil;

/**
 * Date and time (usually with seconds)
 */
public class Timestamp extends Date {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 4574696935450819323L;

	/**
	 * 
	 */
	public Timestamp() {
		super();
		TimeUtil.clearMillisecondComponent(this);
	}

	/**
	 * 
	 * @param date
	 */
	public Timestamp(long date) {
		super(date);
		TimeUtil.clearMillisecondComponent(this);
	}

	/**
	 * 
	 * @param date
	 */
	public Timestamp(Date date) {
		this(date.getTime());
	}
	
	/**
	 * 
	 * @param serializedForm
	 * @throws ParseException
	 */
	public Timestamp(String serializedForm) throws ParseException {
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

	public Timestamp setLocalDate(LocalDate date) {
		setTime(date.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
		return this; // for EL or method chaining
	}

	public Timestamp setLocalDateTime(LocalDateTime date) {
		setTime(date.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
		TimeUtil.clearMillisecondComponent(this);
		return this; // for EL or method chaining
	}
}
