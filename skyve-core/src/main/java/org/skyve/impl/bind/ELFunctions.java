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

import jakarta.annotation.Nonnull;

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
	public static @Nonnull DateOnly newDateOnly() {
		return new DateOnly();
	}
	
	/**
	 * Creates a {@link DateOnly} from epoch milliseconds.
	 *
	 * @param value epoch milliseconds
	 * @return the converted date-only value
	 */
	public static @Nonnull DateOnly newDateOnlyFromMillis(long value) {
		return new DateOnly(value);
	}
	
	/**
	 * Creates a {@link DateOnly} from a {@link Date}.
	 *
	 * @param value the source date
	 * @return the converted date-only value
	 */
	public static @Nonnull DateOnly newDateOnlyFromDate(@Nonnull Date value) {
		return new DateOnly(value);
	}
	
	/**
	 * Creates a {@link DateOnly} from its serialized textual form.
	 *
	 * @param value the serialized value
	 * @return the parsed date-only value
	 * @throws ParseException if the input cannot be parsed
	 */
	public static @Nonnull DateOnly newDateOnlyFromSerializedForm(@Nonnull String value) throws ParseException {
		return new DateOnly(value);
	}
	
	/**
	 * Creates a {@link DateOnly} from a {@link LocalDate}.
	 *
	 * @param value the source local date
	 * @return the converted date-only value
	 */
	public static @Nonnull DateOnly newDateOnlyFromLocalDate(@Nonnull LocalDate value) {
		return new DateOnly(value);
	}

	/**
	 * Creates a {@link DateOnly} from a {@link LocalDateTime}.
	 *
	 * @param value the source local date-time
	 * @return the converted date-only value
	 */
	public static @Nonnull DateOnly newDateOnlyFromLocalDateTime(@Nonnull LocalDateTime value) {
		return new DateOnly(value);
	}

	/**
	 * Creates a new {@link DateTime} using the current clock.
	 *
	 * @return a new date-time value
	 */
	public static @Nonnull DateTime newDateTime() {
		return new DateTime();
	}
	
	/**
	 * Creates a {@link DateTime} from epoch milliseconds.
	 *
	 * @param value epoch milliseconds
	 * @return the converted date-time value
	 */
	public static @Nonnull DateTime newDateTimeFromMillis(long value) {
		return new DateTime(value);
	}
	
	/**
	 * Creates a {@link DateTime} from a {@link Date}.
	 *
	 * @param value the source date
	 * @return the converted date-time value
	 */
	public static @Nonnull DateTime newDateTimeFromDate(@Nonnull Date value) {
		return new DateTime(value);
	}
	
	/**
	 * Creates a {@link DateTime} from its serialized textual form.
	 *
	 * @param value the serialized value
	 * @return the parsed date-time value
	 * @throws ParseException if the input cannot be parsed
	 */
	public static @Nonnull DateTime newDateTimeFromSerializedForm(@Nonnull String value) throws ParseException {
		return new DateTime(value);
	}
	
	/**
	 * Creates a {@link DateTime} from a {@link LocalDate}.
	 *
	 * @param value the source local date
	 * @return the converted date-time value
	 */
	public static @Nonnull DateTime newDateTimeFromLocalDate(@Nonnull LocalDate value) {
		return new DateTime(value);
	}

	/**
	 * Creates a {@link DateTime} from a {@link LocalDateTime}.
	 *
	 * @param value the source local date-time
	 * @return the converted date-time value
	 */
	public static @Nonnull DateTime newDateTimeFromLocalDateTime(@Nonnull LocalDateTime value) {
		return new DateTime(value);
	}

	/**
	 * Creates a new {@link TimeOnly} using the current clock.
	 *
	 * @return a new time-only value
	 */
	public static @Nonnull TimeOnly newTimeOnly() {
		return new TimeOnly();
	}
	
	/**
	 * Creates a {@link TimeOnly} from epoch milliseconds.
	 *
	 * @param value epoch milliseconds
	 * @return the converted time-only value
	 */
	public static @Nonnull TimeOnly newTimeOnlyFromMillis(long value) {
		return new TimeOnly(value);
	}
	
	/**
	 * Creates a {@link TimeOnly} from a {@link Date}.
	 *
	 * @param value the source date
	 * @return the converted time-only value
	 */
	public static @Nonnull TimeOnly newTimeOnlyFromDate(@Nonnull Date value) {
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
	public static @Nonnull TimeOnly newTimeOnlyFromComponents(int hours24, int minutes, int seconds) {
		return new TimeOnly(hours24, minutes, seconds);
	}
	
	/**
	 * Creates a {@link TimeOnly} from its serialized textual form.
	 *
	 * @param value the serialized value
	 * @return the parsed time-only value
	 * @throws ParseException if the input cannot be parsed
	 */
	public static @Nonnull TimeOnly newTimeOnlyFromSerializedForm(@Nonnull String value) throws ParseException {
		return new TimeOnly(value);
	}
	
	/**
	 * Creates a {@link TimeOnly} from a {@link LocalTime}.
	 *
	 * @param value the source local time
	 * @return the converted time-only value
	 */
	public static @Nonnull TimeOnly newTimeOnlyFromLocalTime(@Nonnull LocalTime value) {
		return new TimeOnly(value);
	}

	/**
	 * Creates a {@link TimeOnly} from a {@link LocalDateTime}.
	 *
	 * @param value the source local date-time
	 * @return the converted time-only value
	 */
	public static @Nonnull TimeOnly newTimeOnlyFromLocalDateTime(@Nonnull LocalDateTime value) {
		return new TimeOnly(value);
	}
	
	/**
	 * Creates a new {@link Timestamp} using the current clock.
	 *
	 * @return a new timestamp value
	 */
	public static @Nonnull Timestamp newTimestamp() {
		return new Timestamp();
	}
	
	/**
	 * Creates a {@link Timestamp} from epoch milliseconds.
	 *
	 * @param value epoch milliseconds
	 * @return the converted timestamp value
	 */
	public static @Nonnull Timestamp newTimestampFromMillis(long value) {
		return new Timestamp(value);
	}
	
	/**
	 * Creates a {@link Timestamp} from a {@link Date}.
	 *
	 * @param value the source date
	 * @return the converted timestamp value
	 */
	public static @Nonnull Timestamp newTimestampFromDate(@Nonnull Date value) {
		return new Timestamp(value);
	}
	
	/**
	 * Creates a {@link Timestamp} from its serialized textual form.
	 *
	 * @param value the serialized value
	 * @return the parsed timestamp value
	 * @throws ParseException if the input cannot be parsed
	 */
	public static @Nonnull Timestamp newTimestampFromSerializedForm(@Nonnull String value) throws ParseException {
		return new Timestamp(value);
	}
	
	/**
	 * Creates a {@link Timestamp} from a {@link LocalDate}.
	 *
	 * @param value the source local date
	 * @return the converted timestamp value
	 */
	public static @Nonnull Timestamp newTimestampFromLocalDate(@Nonnull LocalDate value) {
		return new Timestamp(value);
	}

	/**
	 * Creates a {@link Timestamp} from a {@link LocalDateTime}.
	 *
	 * @param value the source local date-time
	 * @return the converted timestamp value
	 */
	public static @Nonnull Timestamp newTimestampFromLocalDateTime(@Nonnull LocalDateTime value) {
		return new Timestamp(value);
	}

	/**
	 * Creates a {@link Decimal2} from a primitive double.
	 *
	 * @param value the source numeric value
	 * @return the converted decimal value
	 */
	public static @Nonnull Decimal2 newDecimal2(double value) {
		return new Decimal2(value);
	}
	
	/**
	 * Creates a {@link Decimal2} from a {@link BigDecimal}.
	 *
	 * @param value the source decimal value
	 * @return the converted decimal value
	 */
	public static @Nonnull Decimal2 newDecimal2FromBigDecimal(@Nonnull BigDecimal value) {
		return new Decimal2(value);
	}

	/**
	 * Creates a {@link Decimal2} from a generic {@link Decimal}.
	 *
	 * @param value the source decimal value
	 * @return the converted decimal value
	 */
	public static @Nonnull Decimal2 newDecimal2FromDecimal(@Nonnull Decimal value) {
		return new Decimal2(value);
	}

	/**
	 * Creates a {@link Decimal2} from textual form.
	 *
	 * @param value the source text value
	 * @return the converted decimal value
	 */
	public static @Nonnull Decimal2 newDecimal2FromString(@Nonnull String value) {
		return new Decimal2(value);
	}

	/**
	 * Creates a {@link Decimal5} from a primitive double.
	 *
	 * @param value the source numeric value
	 * @return the converted decimal value
	 */
	public static @Nonnull Decimal5 newDecimal5(double value) {
		return new Decimal5(value);
	}
	
	/**
	 * Creates a {@link Decimal5} from a {@link BigDecimal}.
	 *
	 * @param value the source decimal value
	 * @return the converted decimal value
	 */
	public static @Nonnull Decimal5 newDecimal5FromBigDecimal(@Nonnull BigDecimal value) {
		return new Decimal5(value);
	}

	/**
	 * Creates a {@link Decimal5} from a generic {@link Decimal}.
	 *
	 * @param value the source decimal value
	 * @return the converted decimal value
	 */
	public static @Nonnull Decimal5 newDecimal5FromDecimal(@Nonnull Decimal value) {
		return new Decimal5(value);
	}

	/**
	 * Creates a {@link Decimal5} from textual form.
	 *
	 * @param value the source text value
	 * @return the converted decimal value
	 */
	public static @Nonnull Decimal5 newDecimal5FromString(@Nonnull String value) {
		return new Decimal5(value);
	}
	
	/**
	 * Creates a {@link Decimal10} from a primitive double.
	 *
	 * @param value the source numeric value
	 * @return the converted decimal value
	 */
	public static @Nonnull Decimal10 newDecimal10(double value) {
		return new Decimal10(value);
	}
	
	/**
	 * Creates a {@link Decimal10} from a {@link BigDecimal}.
	 *
	 * @param value the source decimal value
	 * @return the converted decimal value
	 */
	public static @Nonnull Decimal10 newDecimal10FromBigDecimal(@Nonnull BigDecimal value) {
		return new Decimal10(value);
	}

	/**
	 * Creates a {@link Decimal10} from a generic {@link Decimal}.
	 *
	 * @param value the source decimal value
	 * @return the converted decimal value
	 */
	public static @Nonnull Decimal10 newDecimal10FromDecimal(@Nonnull Decimal value) {
		return new Decimal10(value);
	}

	/**
	 * Creates a {@link Decimal10} from textual form.
	 *
	 * @param value the source text value
	 * @return the converted decimal value
	 */
	public static @Nonnull Decimal10 newDecimal10FromString(@Nonnull String value) {
		return new Decimal10(value);
	}
	
	/**
	 * Creates an {@link OptimisticLock} from username and timestamp components.
	 *
	 * @param username the user component
	 * @param timestamp the timestamp component
	 * @return the constructed optimistic lock value
	 */
	public static @Nonnull OptimisticLock newOptimisticLock(@Nonnull String username, @Nonnull Date timestamp) {
		return new OptimisticLock(username, timestamp);
	}

	/**
	 * Creates an {@link OptimisticLock} from its serialized textual form.
	 *
	 * @param value the serialized value
	 * @return the parsed optimistic lock value
	 */
	public static @Nonnull OptimisticLock newOptimisticLockFromString(@Nonnull String value) {
		return new OptimisticLock(value);
	}
	
	/**
	 * Parses WKT text into a geometry instance.
	 *
	 * @param wkt the well-known text representation
	 * @return the parsed geometry
	 * @throws org.locationtech.jts.io.ParseException if WKT is invalid
	 */
	public static @Nonnull Geometry newGeometry(@Nonnull String wkt) throws org.locationtech.jts.io.ParseException {
		return new WKTReader().read(wkt);
	}
}
