package org.skyve.domain.types;

import java.text.ParseException;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;

import org.skyve.CORE;
import org.skyve.impl.util.TimeUtil;

/**
 * A {@link java.util.Date} that carries only a wall-clock time (hours, minutes, seconds)
 * — the date component is reset to the epoch ({@code 1970-01-01}) and milliseconds are
 * always zeroed.
 *
 * <p>Corresponds to Skyve's {@code time} attribute type. Serialization uses the format
 * returned by {@link org.skyve.CORE#getSerializableTimeFormat()} (e.g. {@code 14:30:00}).
 *
 * <p>Threading: not thread-safe.
 *
 * @see DateOnly
 * @see DateTime
 */
public class TimeOnly extends Date {
	private static final long serialVersionUID = -855625417592920463L;

	/**
	 * Constructs a {@code TimeOnly} representing the current wall-clock time,
	 * truncated to seconds and with the date component reset to the epoch.
	 */
	public TimeOnly() {
		super();
		TimeUtil.clearDateComponent(this);
		TimeUtil.clearMillisecondComponent(this);
	}

	/**
	 * Constructs a {@code TimeOnly} from an epoch-millisecond value,
	 * extracting the time of day and resetting the date to the epoch.
	 *
	 * @param date epoch milliseconds
	 */
	public TimeOnly(long date) {
		super(date);
		TimeUtil.clearDateComponent(this);
		TimeUtil.clearMillisecondComponent(this);
	}

	/**
	 * Constructs a {@code TimeOnly} from an existing {@link java.util.Date},
	 * extracting the time of day and resetting the date to the epoch.
	 *
	 * @param date the source date; must not be {@code null}
	 */
	public TimeOnly(Date date) {
		this(date.getTime());
	}
	
	/**
	 * Constructs a {@code TimeOnly} from explicit hour, minute, and second components.
	 *
	 * @param hours24  hour in 24-hour format (0–23)
	 * @param minutes  minute (0–59)
	 * @param seconds  second (0–59)
	 */
	public TimeOnly(int hours24, int minutes, int seconds) {
		super(0);
		TimeUtil.setTime(this, hours24, minutes, seconds);
	}

	/**
	 * Constructs a {@code TimeOnly} by parsing a string produced by {@link #toString()}
	 * or {@link org.skyve.CORE#getSerializableTimeFormat()}.
	 *
	 * @param serializedForm the time string; must not be {@code null}
	 * @throws java.text.ParseException if {@code serializedForm} does not match the format
	 */
	public TimeOnly(String serializedForm) throws ParseException {
		this(CORE.getSerializableTimeFormat().parse(serializedForm).getTime());
	}

	/**
	 * Constructs a {@code TimeOnly} from a {@link java.time.LocalTime}.
	 *
	 * @param time the local time; must not be {@code null}
	 */
	public TimeOnly(LocalTime time) {
		this(time.getHour(), time.getMinute(), time.getSecond());
	}

	/**
	 * Constructs a {@code TimeOnly} from a {@link java.time.LocalDateTime},
	 * retaining only the time-of-day portion.
	 *
	 * @param date the local date-time; must not be {@code null}
	 */
	public TimeOnly(LocalDateTime date) {
		this(date.toLocalTime());
	}

	/**
	 * Returns a string representation of this instance.
	 * @return the result
	 */
	@Override
	public String toString() {
		return CORE.getSerializableTimeFormat().format(this);
	}
	
	/**
	 * Executes toLocalTime.
	 * @return the result
	 */
	public LocalTime toLocalTime() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalTime();
	}
}
