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

public class ELFunctions {
	private ELFunctions() {
		// disallow instantiation
	}
	
	public static DateOnly newDateOnly() {
		return new DateOnly();
	}
	
	public static DateOnly newDateOnlyFromMillis(long value) {
		return new DateOnly(value);
	}
	
	public static DateOnly newDateOnlyFromDate(Date value) {
		return new DateOnly(value);
	}
	
	public static DateOnly newDateOnlyFromSerializedForm(String value) throws ParseException {
		return new DateOnly(value);
	}
	
	public static DateOnly newDateOnlyFromLocalDate(LocalDate value) {
		return new DateOnly(value);
	}

	public static DateOnly newDateOnlyFromLocalDateTime(LocalDateTime value) {
		return new DateOnly(value);
	}

	public static DateTime newDateTime() {
		return new DateTime();
	}
	
	public static DateTime newDateTimeFromMillis(long value) {
		return new DateTime(value);
	}
	
	public static DateTime newDateTimeFromDate(Date value) {
		return new DateTime(value);
	}
	
	public static DateTime newDateTimeFromSerializedForm(String value) throws ParseException {
		return new DateTime(value);
	}
	
	public static DateTime newDateTimeFromLocalDate(LocalDate value) {
		return new DateTime(value);
	}

	public static DateTime newDateTimeFromLocalDateTime(LocalDateTime value) {
		return new DateTime(value);
	}

	public static TimeOnly newTimeOnly() {
		return new TimeOnly();
	}
	
	public static TimeOnly newTimeOnlyFromMillis(long value) {
		return new TimeOnly(value);
	}
	
	public static TimeOnly newTimeOnlyFromDate(Date value) {
		return new TimeOnly(value);
	}
	
	public static TimeOnly newTimeOnlyFromComponents(int hours24, int minutes, int seconds) {
		return new TimeOnly(hours24, minutes, seconds);
	}
	
	public static TimeOnly newTimeOnlyFromSerializedForm(String value) throws ParseException {
		return new TimeOnly(value);
	}
	
	public static TimeOnly newTimeOnlyFromLocalTime(LocalTime value) {
		return new TimeOnly(value);
	}

	public static TimeOnly newTimeOnlyFromLocalDateTime(LocalDateTime value) {
		return new TimeOnly(value);
	}
	
	public static Timestamp newTimestamp() {
		return new Timestamp();
	}
	
	public static Timestamp newTimestampFromMillis(long value) {
		return new Timestamp(value);
	}
	
	public static Timestamp newTimestampFromDate(Date value) {
		return new Timestamp(value);
	}
	
	public static Timestamp newTimestampFromSerializedForm(String value) throws ParseException {
		return new Timestamp(value);
	}
	
	public static Timestamp newTimestampFromLocalDate(LocalDate value) {
		return new Timestamp(value);
	}

	public static Timestamp newTimestampFromLocalDateTime(LocalDateTime value) {
		return new Timestamp(value);
	}

	public static Decimal2 newDecimal2(double value) {
		return new Decimal2(value);
	}
	
	public static Decimal2 newDecimal2FromBigDecimal(BigDecimal value) {
		return new Decimal2(value);
	}

	public static Decimal2 newDecimal2FromDecimal(Decimal value) {
		return new Decimal2(value);
	}

	public static Decimal2 newDecimal2FromString(String value) {
		return new Decimal2(value);
	}

	public static Decimal5 newDecimal5(double value) {
		return new Decimal5(value);
	}
	
	public static Decimal5 newDecimal5FromBigDecimal(BigDecimal value) {
		return new Decimal5(value);
	}

	public static Decimal5 newDecimal5FromDecimal(Decimal value) {
		return new Decimal5(value);
	}

	public static Decimal5 newDecimal5FromString(String value) {
		return new Decimal5(value);
	}
	
	public static Decimal10 newDecimal10(double value) {
		return new Decimal10(value);
	}
	
	public static Decimal10 newDecimal10FromBigDecimal(BigDecimal value) {
		return new Decimal10(value);
	}

	public static Decimal10 newDecimal10FromDecimal(Decimal value) {
		return new Decimal10(value);
	}

	public static Decimal10 newDecimal10FromString(String value) {
		return new Decimal10(value);
	}
	
	public static OptimisticLock newOptimisticLock(String username, Date timestamp) {
		return new OptimisticLock(username, timestamp);
	}

	public static OptimisticLock newOptimisticLockFromString(String value) {
		return new OptimisticLock(value);
	}
	
	public static Geometry newGeometry(String wkt) throws org.locationtech.jts.io.ParseException {
		return new WKTReader().read(wkt);
	}
}
