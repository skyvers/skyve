package org.skyve.domain.types;

import java.text.ParseException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;

import org.skyve.CORE;
import org.skyve.impl.util.TimeUtil;

/**
 * A {@link java.util.Date} that carries only a calendar date — the time component is
 * always zeroed to midnight in the JVM's default time zone.
 *
 * <p>Corresponds to Skyve's {@code date} attribute type in document metadata. When
 * persisted, only the date portion is stored; the time component is discarded. When
 * loaded, the time component is reset to midnight by
 * {@link org.skyve.impl.util.TimeUtil#clearTimeComponent}.
 *
 * <p>Serialization: {@link #toString()} and the {@link #DateOnly(String)} constructor
 * use the format returned by {@link org.skyve.CORE#getSerializableDateFormat()}
 * (typically {@code dd-MMM-yyyy}, e.g. {@code 01-Jan-2024}).
 *
 * <p>Threading: not thread-safe; do not share instances across threads.
 */
public class DateOnly extends Date {
	private static final long serialVersionUID = 3895082658495913979L;

	/**
	 * Constructs a {@code DateOnly} representing the current date at midnight.
	 */
	public DateOnly() {
		super();
		TimeUtil.clearTimeComponent(this);
	}

	/**
	 * Constructs a {@code DateOnly} from an existing {@link java.util.Date},
	 * discarding the time component.
	 *
	 * @param date the source date; must not be {@code null}
	 */
	public DateOnly(Date date) {
		this(date.getTime());
	}
	
	/**
	 * Constructs a {@code DateOnly} from an epoch-millisecond value,
	 * discarding the time component.
	 *
	 * @param date epoch milliseconds
	 */
	public DateOnly(long date) {
		super(date);
		TimeUtil.clearTimeComponent(this);
	}

	/**
	 * Constructs a {@code DateOnly} by parsing a string produced by
	 * {@link #toString()} or {@link org.skyve.CORE#getSerializableDateFormat()}.
	 *
	 * @param serializedForm the date string; must not be {@code null}
	 * @throws java.text.ParseException if {@code serializedForm} does not match the format
	 */
	public DateOnly(String serializedForm) throws ParseException {
		this(CORE.getSerializableDateFormat().parse(serializedForm).getTime());
	}

	/**
	 * Constructs a {@code DateOnly} from a {@link java.time.LocalDate} at midnight
	 * in the JVM's default time zone.
	 *
	 * @param date the local date; must not be {@code null}
	 */
	public DateOnly(LocalDate date) {
		this(date.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
	}
	
	/**
	 * Constructs a {@code DateOnly} from a {@link java.time.LocalDateTime}, retaining
	 * only the date part at midnight in the JVM's default time zone.
	 *
	 * @param date the local date-time; must not be {@code null}
	 */
	public DateOnly(LocalDateTime date) {
		this(date.toLocalDate().atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
	}
	
	/**
	 * Returns this date as a string in the serializable date format
	 * (e.g. {@code 01-Jan-2024}).
	 *
	 * @return the formatted date string; never {@code null}
	 */
	@Override
	public String toString() {
		return CORE.getSerializableDateFormat().format(this);
	}
	
	/**
	 * Converts this date to a {@link java.time.LocalDate} in the JVM's default time zone.
	 *
	 * @return the equivalent {@code LocalDate}; never {@code null}
	 */
	public LocalDate toLocalDate() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
	}

	/**
	 * Converts this date to a {@link java.time.LocalDateTime} at midnight
	 * in the JVM's default time zone.
	 *
	 * @return the equivalent {@code LocalDateTime}; never {@code null}
	 */
	public LocalDateTime toLocalDateTime() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
	}
}
