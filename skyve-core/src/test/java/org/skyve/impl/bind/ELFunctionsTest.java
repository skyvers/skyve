package org.skyve.impl.bind;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.math.BigDecimal;
import java.text.ParseException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Date;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;

public class ELFunctionsTest {

	@Test
	@SuppressWarnings("static-method")
	public void newDateOnly() {
		assertNotNull(ELFunctions.newDateOnly());
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDateOnlyFromMillis() {
		DateOnly d = ELFunctions.newDateOnlyFromMillis(System.currentTimeMillis());
		assertNotNull(d);
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDateOnlyFromDate() {
		assertNotNull(ELFunctions.newDateOnlyFromDate(new Date()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDateOnlyFromSerializedForm() throws ParseException {
		assertNotNull(ELFunctions.newDateOnlyFromSerializedForm("2024-01-15"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDateOnlyFromLocalDate() {
		assertNotNull(ELFunctions.newDateOnlyFromLocalDate(LocalDate.of(2024, 1, 15)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDateOnlyFromLocalDateTime() {
		assertNotNull(ELFunctions.newDateOnlyFromLocalDateTime(LocalDateTime.of(2024, 1, 15, 10, 0)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDateTime() {
		assertNotNull(ELFunctions.newDateTime());
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDateTimeFromMillis() {
		assertNotNull(ELFunctions.newDateTimeFromMillis(System.currentTimeMillis()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDateTimeFromDate() {
		assertNotNull(ELFunctions.newDateTimeFromDate(new Date()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDateTimeFromSerializedForm() throws ParseException {
		DateTime dt = ELFunctions.newDateTimeFromSerializedForm("2024-01-15T10:00:00");
		assertNotNull(dt);
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDateTimeFromLocalDate() {
		assertNotNull(ELFunctions.newDateTimeFromLocalDate(LocalDate.of(2024, 6, 1)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDateTimeFromLocalDateTime() {
		assertNotNull(ELFunctions.newDateTimeFromLocalDateTime(LocalDateTime.of(2024, 6, 1, 12, 0)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newTimeOnly() {
		assertNotNull(ELFunctions.newTimeOnly());
	}

	@Test
	@SuppressWarnings("static-method")
	public void newTimeOnlyFromMillis() {
		assertNotNull(ELFunctions.newTimeOnlyFromMillis(0L));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newTimeOnlyFromDate() {
		assertNotNull(ELFunctions.newTimeOnlyFromDate(new Date()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newTimeOnlyFromComponents() {
		TimeOnly t = ELFunctions.newTimeOnlyFromComponents(10, 30, 0);
		assertNotNull(t);
	}

	@Test
	@SuppressWarnings("static-method")
	public void newTimeOnlyFromSerializedForm() throws ParseException {
		assertNotNull(ELFunctions.newTimeOnlyFromSerializedForm("10:30:00"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newTimeOnlyFromLocalTime() {
		assertNotNull(ELFunctions.newTimeOnlyFromLocalTime(LocalTime.of(10, 30)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newTimeOnlyFromLocalDateTime() {
		assertNotNull(ELFunctions.newTimeOnlyFromLocalDateTime(LocalDateTime.of(2024, 6, 1, 10, 30)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newTimestamp() {
		assertNotNull(ELFunctions.newTimestamp());
	}

	@Test
	@SuppressWarnings("static-method")
	public void newTimestampFromMillis() {
		assertNotNull(ELFunctions.newTimestampFromMillis(System.currentTimeMillis()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newTimestampFromDate() {
		assertNotNull(ELFunctions.newTimestampFromDate(new Date()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newTimestampFromSerializedForm() throws ParseException {
		assertNotNull(ELFunctions.newTimestampFromSerializedForm("2024-01-15T10:00:00"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newTimestampFromLocalDate() {
		assertNotNull(ELFunctions.newTimestampFromLocalDate(LocalDate.of(2024, 1, 1)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newTimestampFromLocalDateTime() {
		assertNotNull(ELFunctions.newTimestampFromLocalDateTime(LocalDateTime.of(2024, 1, 1, 0, 0)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDecimal2FromDouble() {
		assertNotNull(ELFunctions.newDecimal2(1.5));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDecimal2FromBigDecimal() {
		assertNotNull(ELFunctions.newDecimal2FromBigDecimal(BigDecimal.ONE));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDecimal2FromDecimal() {
		Decimal2 base = new Decimal2(2.0);
		assertNotNull(ELFunctions.newDecimal2FromDecimal(base));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDecimal2FromString() {
		assertNotNull(ELFunctions.newDecimal2FromString("3.14"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDecimal5FromDouble() {
		assertNotNull(ELFunctions.newDecimal5(1.5));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDecimal5FromBigDecimal() {
		assertNotNull(ELFunctions.newDecimal5FromBigDecimal(BigDecimal.TEN));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDecimal5FromDecimal() {
		Decimal5 base = new Decimal5(2.0);
		assertNotNull(ELFunctions.newDecimal5FromDecimal(base));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDecimal5FromString() {
		assertNotNull(ELFunctions.newDecimal5FromString("1.23456"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDecimal10FromDouble() {
		assertNotNull(ELFunctions.newDecimal10(1.5));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDecimal10FromBigDecimal() {
		assertNotNull(ELFunctions.newDecimal10FromBigDecimal(BigDecimal.valueOf(100)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDecimal10FromDecimal() {
		Decimal10 base = new Decimal10(2.0);
		assertNotNull(ELFunctions.newDecimal10FromDecimal(base));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newDecimal10FromString() {
		assertNotNull(ELFunctions.newDecimal10FromString("9.99"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newOptimisticLock() {
		assertNotNull(ELFunctions.newOptimisticLock("admin", new Date()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newOptimisticLockFromString() throws Exception {
		// Format: 17 chars for timestamp + username
		// LOCK_TIMESTAMP_FORMAT = "yyyyMMddHHmmssSSS"
		// Build a valid lock string: 17-char date + username
		String lockString = "20230615103000000admin";
		assertNotNull(ELFunctions.newOptimisticLockFromString(lockString));
	}

	@Test
	@SuppressWarnings("static-method")
	public void newGeometry() throws Exception {
		assertNotNull(ELFunctions.newGeometry("POINT (0 0)"));
	}
}
