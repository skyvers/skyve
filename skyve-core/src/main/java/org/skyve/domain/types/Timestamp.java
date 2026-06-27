package org.skyve.domain.types;

import java.text.ParseException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;

import org.skyve.impl.util.TimeUtil;

/**
 * A {@link java.util.Date} that carries a date and time to the <em>second</em> —
 * milliseconds are always zeroed.
 *
 * <p>Corresponds to Skyve's {@code timestamp} attribute type. Serialization uses an
 * ISO 8601-style format (e.g. {@code 2024-01-15T14:30:45}) via
 * {@link org.skyve.impl.util.TimeUtil#formatISODate}.
 *
 * <p>Threading: not thread-safe.
 *
 * @see DateTime
 * @see DateOnly
 */
public class Timestamp extends Date {
	private static final long serialVersionUID = 4574696935450819323L;

	/**
	 * Creates a new Timestamp instance.
	 */
	public Timestamp() {
		super();
		TimeUtil.clearMillisecondComponent(this);
	}

	/**
	 * Creates a new Timestamp instance.
	 * @param date the date
	 */
	public Timestamp(long date) {
		super(date);
		TimeUtil.clearMillisecondComponent(this);
	}

	/**
	 * Creates a new Timestamp instance.
	 * @param date the date
	 */
	public Timestamp(Date date) {
		this(date.getTime());
	}
	
	/**
	 * Creates a new Timestamp instance.
	 * @param serializedForm the serializedForm
	 */
	public Timestamp(String serializedForm) throws ParseException {
		this(TimeUtil.parseISODate(serializedForm));
	}

	/**
	 * Creates a new Timestamp instance.
	 * @param date the date
	 */
	public Timestamp(LocalDate date) {
		this(date.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
	}
	
	/**
	 * Creates a new Timestamp instance.
	 * @param date the date
	 */
	public Timestamp(LocalDateTime date) {
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
