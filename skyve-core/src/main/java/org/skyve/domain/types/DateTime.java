package org.skyve.domain.types;

import java.text.ParseException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;

import org.skyve.impl.util.TimeUtil;

/**
 * A {@link java.util.Date} that carries a date and time to the <em>minute</em> —
 * seconds and milliseconds are always zeroed.
 *
 * <p>Corresponds to Skyve's {@code dateTime} attribute type. Serialization uses an
 * ISO 8601-style format (e.g. {@code 2024-01-15T14:30}) via
 * {@link org.skyve.impl.util.TimeUtil#formatISODate}.
 *
 * <p>Threading: not thread-safe.
 *
 * @see DateOnly
 * @see Timestamp
 */
public class DateTime extends Date {
	private static final long serialVersionUID = -3775522231628914522L;

	/**
	 * Creates a new DateTime instance.
	 */
	public DateTime() {
		super();
		TimeUtil.clearSecondAndMillisecondComponent(this);
	}

	/**
	 * Creates a new DateTime instance.
	 * @param date the date
	 */
	public DateTime(long date) {
		super(date);
		TimeUtil.clearMillisecondComponent(this);
	}
	
	/**
	 * Creates a new DateTime instance.
	 * @param date the date
	 */
	public DateTime(Date date) {
		this(date.getTime());
	}
	
	/**
	 * Creates a new DateTime instance.
	 * @param serializedForm the serializedForm
	 */
	public DateTime(String serializedForm) throws ParseException {
		this(TimeUtil.parseISODate(serializedForm));
	}

	/**
	 * Creates a new DateTime instance.
	 * @param date the date
	 */
	public DateTime(LocalDate date) {
		this(date.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
	}
	
	/**
	 * Creates a new DateTime instance.
	 * @param date the date
	 */
	public DateTime(LocalDateTime date) {
		this(date.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
	}
	
	/**
	 * Returns a string representation of this instance.
	 * @return the result
	 */
	@Override
	public String toString() {
		return TimeUtil.formatISODate(this, true);
	}
	
	/**
	 * Executes toLocalDate.
	 * @return the result
	 */
	public LocalDate toLocalDate() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
	}

	/**
	 * Executes toLocalDateTime.
	 * @return the result
	 */
	public LocalDateTime toLocalDateTime() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
	}

	/**
	 * Executes toLocalTime.
	 * @return the result
	 */
	public LocalTime toLocalTime() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalTime();
	}
}
