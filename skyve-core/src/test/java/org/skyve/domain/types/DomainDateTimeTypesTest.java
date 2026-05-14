package org.skyve.domain.types;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Date;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link DateOnly}, {@link TimeOnly}, {@link DateTime}, and {@link Timestamp}.
 */
@SuppressWarnings("static-method")
class DomainDateTimeTypesTest {

	// ---- DateOnly constructors -------------------------------------------

	@Test
	void dateOnlyDefaultConstructorClearsTime() {
		DateOnly d = new DateOnly();
		// toLocalDate() returns the date portion; hours should be at midnight
		assertNotNull(d.toLocalDate());
	}

	@Test
	void dateOnlyFromLongConstructorWorks() {
		long epochMs = LocalDate.of(2024, 6, 15).atStartOfDay()
				.atZone(java.time.ZoneId.systemDefault()).toInstant().toEpochMilli();
		DateOnly d = new DateOnly(epochMs);
		assertEquals(LocalDate.of(2024, 6, 15), d.toLocalDate());
	}

	@Test
	void dateOnlyFromDateConstructorWorks() {
		Date src = new Date(0);
		DateOnly d = new DateOnly(src);
		assertNotNull(d.toLocalDate());
	}

	@Test
	void dateOnlyFromLocalDateConstructorWorks() {
		LocalDate ld = LocalDate.of(2023, 3, 10);
		DateOnly d = new DateOnly(ld);
		assertEquals(ld, d.toLocalDate());
	}

	@Test
	void dateOnlyFromLocalDateTimeConstructorWorks() {
		LocalDateTime ldt = LocalDateTime.of(2023, 3, 10, 14, 30);
		DateOnly d = new DateOnly(ldt);
		assertEquals(ldt.toLocalDate(), d.toLocalDate());
	}

	@Test
	void dateOnlyFromStringConstructorWorks() throws Exception {
		DateOnly d = new DateOnly("2024-01-15");
		assertEquals(LocalDate.of(2024, 1, 15), d.toLocalDate());
	}

	@Test
	void dateOnlyToStringReturnsIsoFormat() {
		DateOnly d = new DateOnly(LocalDate.of(2024, 6, 1));
		assertEquals("2024-06-01", d.toString());
	}

	@Test
	void dateOnlyToLocalDateTimeWorks() {
		DateOnly d = new DateOnly(LocalDate.of(2024, 6, 1));
		LocalDateTime result = d.toLocalDateTime();
		assertNotNull(result);
		assertEquals(2024, result.getYear());
		assertEquals(6, result.getMonthValue());
		assertEquals(1, result.getDayOfMonth());
	}

	// ---- TimeOnly constructors ------------------------------------------

	@Test
	void timeOnlyDefaultConstructorClearsDate() {
		TimeOnly t = new TimeOnly();
		assertNotNull(t.toLocalTime());
	}

	@Test
	void timeOnlyFromHoursMinutesSecondsConstructorWorks() {
		TimeOnly t = new TimeOnly(14, 30, 45);
		LocalTime lt = t.toLocalTime();
		assertEquals(14, lt.getHour());
		assertEquals(30, lt.getMinute());
		assertEquals(45, lt.getSecond());
	}

	@Test
	void timeOnlyFromDateConstructorWorks() {
		TimeOnly t = new TimeOnly(new Date(0));
		assertNotNull(t.toLocalTime());
	}

	@Test
	void timeOnlyFromLocalTimeConstructorWorks() {
		LocalTime src = LocalTime.of(9, 15, 0);
		TimeOnly t = new TimeOnly(src);
		assertEquals(9, t.toLocalTime().getHour());
		assertEquals(15, t.toLocalTime().getMinute());
	}

	@Test
	void timeOnlyFromLocalDateTimeConstructorWorks() {
		LocalDateTime ldt = LocalDateTime.of(2024, 1, 1, 10, 20, 30);
		TimeOnly t = new TimeOnly(ldt);
		assertEquals(10, t.toLocalTime().getHour());
		assertEquals(20, t.toLocalTime().getMinute());
	}

	@Test
	void timeOnlyFromStringConstructorWorks() throws Exception {
		TimeOnly t = new TimeOnly("08:45:00");
		assertEquals(8, t.toLocalTime().getHour());
		assertEquals(45, t.toLocalTime().getMinute());
	}

	@Test
	void timeOnlyToStringReturnsHHMMSS() {
		TimeOnly t = new TimeOnly(9, 5, 3);
		String s = t.toString();
		assertNotNull(s);
		// format HH:mm:ss
		assertEquals(8, s.length());
	}

	@Test
	void timeOnlyFromLongConstructorWorks() {
		TimeOnly t = new TimeOnly(0L);
		assertNotNull(t.toLocalTime());
	}

	// ---- DateTime constructors ------------------------------------------

	@Test
	void dateTimeDefaultConstructorWorks() {
		DateTime dt = new DateTime();
		assertNotNull(dt.toLocalDateTime());
	}

	@Test
	void dateTimeFromLongConstructorWorks() {
		long epochMs = LocalDateTime.of(2024, 5, 10, 12, 30)
				.atZone(java.time.ZoneId.systemDefault()).toInstant().toEpochMilli();
		DateTime dt = new DateTime(epochMs);
		LocalDateTime result = dt.toLocalDateTime();
		assertEquals(2024, result.getYear());
		assertEquals(5, result.getMonthValue());
	}

	@Test
	void dateTimeFromDateConstructorWorks() {
		DateTime dt = new DateTime(new Date(0));
		assertNotNull(dt.toLocalDateTime());
	}

	@Test
	void dateTimeFromLocalDateConstructorWorks() {
		LocalDate ld = LocalDate.of(2024, 3, 20);
		DateTime dt = new DateTime(ld);
		assertEquals(ld, dt.toLocalDate());
	}

	@Test
	void dateTimeFromLocalDateTimeConstructorWorks() {
		LocalDateTime ldt = LocalDateTime.of(2024, 7, 4, 9, 0);
		DateTime dt = new DateTime(ldt);
		assertEquals(2024, dt.toLocalDate().getYear());
		assertEquals(7, dt.toLocalDate().getMonthValue());
	}

	@Test
	void dateTimeFromStringConstructorWorks() throws Exception {
		DateTime dt = new DateTime("2024-04-01T10:00:00");
		assertNotNull(dt.toLocalDate());
	}

	@Test
	void dateTimeToStringIsNotNull() {
		DateTime dt = new DateTime(LocalDateTime.of(2024, 1, 1, 8, 0));
		assertNotNull(dt.toString());
	}

	@Test
	void dateTimeToLocalDateWorks() {
		DateTime dt = new DateTime(LocalDateTime.of(2024, 2, 14, 12, 0));
		assertEquals(LocalDate.of(2024, 2, 14), dt.toLocalDate());
	}

	@Test
	void dateTimeToLocalTimeWorks() {
		DateTime dt = new DateTime(LocalDateTime.of(2024, 1, 1, 15, 45, 0));
		LocalTime lt = dt.toLocalTime();
		assertNotNull(lt);
		assertEquals(15, lt.getHour());
		assertEquals(45, lt.getMinute());
	}

	// ---- Timestamp constructors -----------------------------------------

	@Test
	void timestampDefaultConstructorWorks() {
		Timestamp ts = new Timestamp();
		assertNotNull(ts.toLocalDateTime());
	}

	@Test
	void timestampFromLongConstructorWorks() {
		long epochMs = LocalDateTime.of(2024, 8, 8, 8, 8, 8)
				.atZone(java.time.ZoneId.systemDefault()).toInstant().toEpochMilli();
		Timestamp ts = new Timestamp(epochMs);
		assertNotNull(ts.toLocalDateTime());
	}

	@Test
	void timestampFromDateConstructorWorks() {
		Timestamp ts = new Timestamp(new Date(0));
		assertNotNull(ts.toLocalDateTime());
	}

	@Test
	void timestampFromLocalDateConstructorWorks() {
		LocalDate ld = LocalDate.of(2024, 9, 9);
		Timestamp ts = new Timestamp(ld);
		assertEquals(ld, ts.toLocalDate());
	}

	@Test
	void timestampFromLocalDateTimeConstructorWorks() {
		LocalDateTime ldt = LocalDateTime.of(2024, 10, 10, 10, 10, 10);
		Timestamp ts = new Timestamp(ldt);
		assertEquals(2024, ts.toLocalDate().getYear());
		assertEquals(10, ts.toLocalDate().getMonthValue());
	}

	@Test
	void timestampFromStringConstructorWorks() throws Exception {
		Timestamp ts = new Timestamp("2024-11-11T11:11:11");
		assertNotNull(ts.toLocalDate());
	}

	@Test
	void timestampToStringIsNotNull() {
		Timestamp ts = new Timestamp(LocalDateTime.of(2024, 1, 1, 0, 0, 0));
		assertNotNull(ts.toString());
	}

	@Test
	void timestampToLocalDateWorks() {
		Timestamp ts = new Timestamp(LocalDateTime.of(2024, 3, 15, 12, 0, 0));
		assertEquals(LocalDate.of(2024, 3, 15), ts.toLocalDate());
	}

	@Test
	void timestampToLocalTimeWorks() {
		Timestamp ts = new Timestamp(LocalDateTime.of(2024, 1, 1, 16, 30, 0));
		LocalTime lt = ts.toLocalTime();
		assertNotNull(lt);
		assertEquals(16, lt.getHour());
		assertEquals(30, lt.getMinute());
	}
}
