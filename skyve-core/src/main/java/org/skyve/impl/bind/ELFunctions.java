package org.skyve.impl.bind;

import java.math.BigDecimal;
import java.text.ParseException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Date;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.WKTReader;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;

/**
 * Provides static constructors exposed to Skyve EL expressions.
 *
 * <p>Function names in this type are part of the EL surface used by
 * {@link ELExpressionEvaluator}.
 */
public class ELFunctions {
	private ELFunctions() {
		// disallow instantiation
	}
	
	/**
	 * Creates a new {@link DateOnly} using the current clock.
	 *
	 * @return a new date-only value
	 */
	public static DateOnly newDateOnly() {
		return new DateOnly();
	}
	
	/**
	 * Creates a {@link DateOnly} from epoch milliseconds.
	 *
	 * @param value epoch milliseconds
	 * @return the converted date-only value
	 */
	public static DateOnly newDateOnlyFromMillis(long value) {
		return new DateOnly(value);
	}
	
	/**
	 * Creates a {@link DateOnly} from a {@link Date}.
	 *
	 * @param value the source date
	 * @return the converted date-only value
	 */
	public static DateOnly newDateOnlyFromDate(Date value) {
		return new DateOnly(value);
	}
	
	/**
	 * Creates a {@link DateOnly} from its serialized textual form.
	 *
	 * @param value the serialized value
	 * @return the parsed date-only value
	 * @throws ParseException if the input cannot be parsed
	 */
	public static DateOnly newDateOnlyFromSerializedForm(String value) throws ParseException {
		return new DateOnly(value);
	}
	
	/**
	 * Creates a {@link DateOnly} from a {@link LocalDate}.
	 *
	 * @param value the source local date
	 * @return the converted date-only value
	 */
	public static DateOnly newDateOnlyFromLocalDate(LocalDate value) {
		return new DateOnly(value);
	}

	/**
	 * Creates a {@link DateOnly} from a {@link LocalDateTime}.
	 *
	 * @param value the source local date-time
	 * @return the converted date-only value
	 */
	public static DateOnly newDateOnlyFromLocalDateTime(LocalDateTime value) {
		return new DateOnly(value);
	}

	/**
	 * Creates a new {@link DateTime} using the current clock.
	 *
	 * @return a new date-time value
	 */
	public static DateTime newDateTime() {
		return new DateTime();
	}
	
	/**
	 * Creates a {@link DateTime} from epoch milliseconds.
	 *
	 * @param value epoch milliseconds
	 * @return the converted date-time value
	 */
	public static DateTime newDateTimeFromMillis(long value) {
		return new DateTime(value);
	}
	
	/**
	 * Creates a {@link DateTime} from a {@link Date}.
	 *
	 * @param value the source date
	 * @return the converted date-time value
	 */
	public static DateTime newDateTimeFromDate(Date value) {
		return new DateTime(value);
	}
	
	/**
	 * Creates a {@link DateTime} from its serialized textual form.
	 *
	 * @param value the serialized value
	 * @return the parsed date-time value
	 * @throws ParseException if the input cannot be parsed
	 */
	public static DateTime newDateTimeFromSerializedForm(String value) throws ParseException {
		return new DateTime(value);
	}
	
	/**
	 * Creates a {@link DateTime} from a {@link LocalDate}.
	 *
	 * @param value the source local date
	 * @return the converted date-time value
	 */
	public static DateTime newDateTimeFromLocalDate(LocalDate value) {
		return new DateTime(value);
	}

	/**
	 * Creates a {@link DateTime} from a {@link LocalDateTime}.
	 *
	 * @param value the source local date-time
	 * @return the converted date-time value
	 */
	public static DateTime newDateTimeFromLocalDateTime(LocalDateTime value) {
		return new DateTime(value);
	}

	/**
	 * Creates a new {@link TimeOnly} using the current clock.
	 *
	 * @return a new time-only value
	 */
	public static TimeOnly newTimeOnly() {
		return new TimeOnly();
	}
	
	/**
	 * Creates a {@link TimeOnly} from epoch milliseconds.
	 *
	 * @param value epoch milliseconds
	 * @return the converted time-only value
	 */
	public static TimeOnly newTimeOnlyFromMillis(long value) {
		return new TimeOnly(value);
	}
	
	/**
	 * Creates a {@link TimeOnly} from a {@link Date}.
	 *
	 * @param value the source date
	 * @return the converted time-only value
	 */
	public static TimeOnly newTimeOnlyFromDate(Date value) {
		return new TimeOnly(value);
	}
	
	/**
	 * Creates a {@link TimeOnly} from hour, minute, and second values.
	 *
	 * @param hours24 hour in 24-hour form
	 * @param minutes minute component
	 * @param seconds second component
	 * @return the constructed time-only value
	 */
	public static TimeOnly newTimeOnlyFromComponents(int hours24, int minutes, int seconds) {
		return new TimeOnly(hours24, minutes, seconds);
	}
	
	/**
	 * Creates a {@link TimeOnly} from its serialized textual form.
	 *
	 * @param value the serialized value
	 * @return the parsed time-only value
	 * @throws ParseException if the input cannot be parsed
	 */
	public static TimeOnly newTimeOnlyFromSerializedForm(String value) throws ParseException {
		return new TimeOnly(value);
	}
	
	/**
	 * Creates a {@link TimeOnly} from a {@link LocalTime}.
	 *
	 * @param value the source local time
	 * @return the converted time-only value
	 */
	public static TimeOnly newTimeOnlyFromLocalTime(LocalTime value) {
		return new TimeOnly(value);
	}

	/**
	 * Creates a {@link TimeOnly} from a {@link LocalDateTime}.
	 *
	 * @param value the source local date-time
	 * @return the converted time-only value
	 */
	public static TimeOnly newTimeOnlyFromLocalDateTime(LocalDateTime value) {
		return new TimeOnly(value);
	}
	
	/**
	 * Creates a new {@link Timestamp} using the current clock.
	 *
	 * @return a new timestamp value
	 */
	public static Timestamp newTimestamp() {
		return new Timestamp();
	}
	
	/**
	 * Creates a {@link Timestamp} from epoch milliseconds.
	 *
	 * @param value epoch milliseconds
	 * @return the converted timestamp value
	 */
	public static Timestamp newTimestampFromMillis(long value) {
		return new Timestamp(value);
	}
	
	/**
	 * Creates a {@link Timestamp} from a {@link Date}.
	 *
	 * @param value the source date
	 * @return the converted timestamp value
	 */
	public static Timestamp newTimestampFromDate(Date value) {
		return new Timestamp(value);
	}
	
	/**
	 * Creates a {@link Timestamp} from its serialized textual form.
	 *
	 * @param value the serialized value
	 * @return the parsed timestamp value
	 * @throws ParseException if the input cannot be parsed
	 */
	public static Timestamp newTimestampFromSerializedForm(String value) throws ParseException {
		return new Timestamp(value);
	}
	
	/**
	 * Creates a {@link Timestamp} from a {@link LocalDate}.
	 *
	 * @param value the source local date
	 * @return the converted timestamp value
	 */
	public static Timestamp newTimestampFromLocalDate(LocalDate value) {
		return new Timestamp(value);
	}

	/**
	 * Creates a {@link Timestamp} from a {@link LocalDateTime}.
	 *
	 * @param value the source local date-time
	 * @return the converted timestamp value
	 */
	public static Timestamp newTimestampFromLocalDateTime(LocalDateTime value) {
		return new Timestamp(value);
	}

	/**
	 * Creates a {@link Decimal2} from a primitive double.
	 *
	 * @param value the source numeric value
	 * @return the converted decimal value
	 */
	public static Decimal2 newDecimal2(double value) {
		return new Decimal2(value);
	}
	
	/**
	 * Creates a {@link Decimal2} from a {@link BigDecimal}.
	 *
	 * @param value the source decimal value
	 * @return the converted decimal value
	 */
	public static Decimal2 newDecimal2FromBigDecimal(BigDecimal value) {
		return new Decimal2(value);
	}

	/**
	 * Creates a {@link Decimal2} from a generic {@link Decimal}.
	 *
	 * @param value the source decimal value
	 * @return the converted decimal value
	 */
	public static Decimal2 newDecimal2FromDecimal(Decimal value) {
		return new Decimal2(value);
	}

	/**
	 * Creates a {@link Decimal2} from textual form.
	 *
	 * @param value the source text value
	 * @return the converted decimal value
	 */
	public static Decimal2 newDecimal2FromString(String value) {
		return new Decimal2(value);
	}

	/**
	 * Creates a {@link Decimal5} from a primitive double.
	 *
	 * @param value the source numeric value
	 * @return the converted decimal value
	 */
	public static Decimal5 newDecimal5(double value) {
		return new Decimal5(value);
	}
	
	/**
	 * Creates a {@link Decimal5} from a {@link BigDecimal}.
	 *
	 * @param value the source decimal value
	 * @return the converted decimal value
	 */
	public static Decimal5 newDecimal5FromBigDecimal(BigDecimal value) {
		return new Decimal5(value);
	}

	/**
	 * Creates a {@link Decimal5} from a generic {@link Decimal}.
	 *
	 * @param value the source decimal value
	 * @return the converted decimal value
	 */
	public static Decimal5 newDecimal5FromDecimal(Decimal value) {
		return new Decimal5(value);
	}

	/**
	 * Creates a {@link Decimal5} from textual form.
	 *
	 * @param value the source text value
	 * @return the converted decimal value
	 */
	public static Decimal5 newDecimal5FromString(String value) {
		return new Decimal5(value);
	}
	
	/**
	 * Creates a {@link Decimal10} from a primitive double.
	 *
	 * @param value the source numeric value
	 * @return the converted decimal value
	 */
	public static Decimal10 newDecimal10(double value) {
		return new Decimal10(value);
	}
	
	/**
	 * Creates a {@link Decimal10} from a {@link BigDecimal}.
	 *
	 * @param value the source decimal value
	 * @return the converted decimal value
	 */
	public static Decimal10 newDecimal10FromBigDecimal(BigDecimal value) {
		return new Decimal10(value);
	}

	/**
	 * Creates a {@link Decimal10} from a generic {@link Decimal}.
	 *
	 * @param value the source decimal value
	 * @return the converted decimal value
	 */
	public static Decimal10 newDecimal10FromDecimal(Decimal value) {
		return new Decimal10(value);
	}

	/**
	 * Creates a {@link Decimal10} from textual form.
	 *
	 * @param value the source text value
	 * @return the converted decimal value
	 */
	public static Decimal10 newDecimal10FromString(String value) {
		return new Decimal10(value);
	}
	
	/**
	 * Creates an {@link OptimisticLock} from username and timestamp components.
	 *
	 * @param username the user component
	 * @param timestamp the timestamp component
	 * @return the constructed optimistic lock value
	 */
	public static OptimisticLock newOptimisticLock(String username, Date timestamp) {
		return new OptimisticLock(username, timestamp);
	}

	/**
	 * Creates an {@link OptimisticLock} from its serialized textual form.
	 *
	 * @param value the serialized value
	 * @return the parsed optimistic lock value
	 */
	public static OptimisticLock newOptimisticLockFromString(String value) {
		return new OptimisticLock(value);
	}
	
	/**
	 * Parses WKT text into a geometry instance.
	 *
	 * @param wkt the well-known text representation
	 * @return the parsed geometry
	 * @throws org.locationtech.jts.io.ParseException if WKT is invalid
	 */
	public static Geometry newGeometry(String wkt) throws org.locationtech.jts.io.ParseException {
		return new WKTReader().read(wkt);
	}
}
