package org.skyve.impl.domain.types;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Time;
import java.sql.Timestamp;
import java.sql.Types;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;

@SuppressWarnings({"resource", "boxing"})
class DateTimeUserTypeTest {

	// A fixed epoch value for tests — 2024-06-15 10:30:45.123 UTC
	private static final long EPOCH = 1718447445123L;

	// ======== TimeOnlyUserType ========

	private final TimeOnlyUserType timeType = new TimeOnlyUserType();

	@Test
	void testTimeOnlySqlTypes() {
		assertArrayEquals(new int[] {Types.TIME}, timeType.sqlTypes());
	}

	@Test
	void testTimeOnlyReturnedClass() {
		assertEquals(TimeOnly.class, timeType.returnedClass());
	}

	@Test
	void testTimeOnlyIsMutable() {
		assertTrue(timeType.isMutable());
	}

	@Test
	void testTimeOnlyEqualsBothNull() {
		// x == y (both null → same ref)
		assertTrue(timeType.equals(null, null));
	}

	@Test
	void testTimeOnlyEqualsXNull() {
		assertFalse(timeType.equals(null, new TimeOnly(EPOCH)));
	}

	@Test
	void testTimeOnlyEqualsYNull() {
		assertFalse(timeType.equals(new TimeOnly(EPOCH), null));
	}

	@Test
	void testTimeOnlyEqualsSameEpoch() {
		assertTrue(timeType.equals(new TimeOnly(EPOCH), new TimeOnly(EPOCH)));
	}

	@Test
	void testTimeOnlyEqualsDistinctTimesDiffer() {
		// Different millis = different time-of-day
		long later = EPOCH + 3600000L; // +1 hour
		assertFalse(timeType.equals(new TimeOnly(EPOCH), new TimeOnly(later)));
	}

	@Test
	void testTimeOnlyEqualsSameHourDifferentMinute() {
		// covers L54: HOUR_OF_DAY equal but MINUTE different
		// TimeOnly(h,m,s) constructor sets time to Jan 1 1970 h:m:s, getTime() differs
		TimeOnly t1 = new TimeOnly(10, 30, 45);
		TimeOnly t2 = new TimeOnly(10, 31, 45);
		assertFalse(timeType.equals(t1, t2));
	}

	@Test
	void testTimeOnlyEqualsSameHourMinuteDifferentSecond() {
		// covers L55: HOUR_OF_DAY and MINUTE equal but SECOND different
		TimeOnly t1 = new TimeOnly(10, 30, 45);
		TimeOnly t2 = new TimeOnly(10, 30, 46);
		assertFalse(timeType.equals(t1, t2));
	}

        @Test
        void testTimeOnlyEqualsSameHMSDifferentMillis() {
                // covers L55 true branch and L56 false branch: HH:MM:SS equal but MILLISECOND differs
                TimeOnly t1 = new TimeOnly(10, 30, 45);
                t1.setTime(t1.getTime() + 100); // add 100ms → MILLISECOND=100
                TimeOnly t2 = new TimeOnly(10, 30, 45);
                // t2 has MILLISECOND=0
                assertFalse(timeType.equals(t1, t2));
        }

        @Test
        void testTimeOnlyEqualsSameHMSAndMillis() {
                // covers L56 true branch: HH:MM:SS:MS all equal
                TimeOnly t1 = new TimeOnly(10, 30, 45);
                t1.setTime(t1.getTime() + 100);
                TimeOnly t2 = new TimeOnly(10, 30, 45);
                t2.setTime(t2.getTime() + 100);
                assertTrue(timeType.equals(t1, t2));
        }

	@Test
	void testTimeOnlyDeepCopyNull() {
		assertNull(timeType.deepCopy(null));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void testTimeOnlyDeepCopyNonNull() {
		TimeOnly t = new TimeOnly(EPOCH);
		Object copy = timeType.deepCopy(t);
		// TimeOnly strips date-component and millis, compare via its own getTime()
		assertEquals(t.getTime(), ((java.util.Date) copy).getTime());
		assertNotSame(t, copy, "deep copy must not be same reference");
	}

	@Test
	void testTimeOnlyDisassemble() {
		TimeOnly t = new TimeOnly(EPOCH);
		assertSame(t, timeType.disassemble(t));
	}

	@Test
	void testTimeOnlyAssemble() {
		TimeOnly t = new TimeOnly(EPOCH);
		assertSame(t, timeType.assemble(t, null));
	}

	@Test
	void testTimeOnlyReplace() {
		TimeOnly original = new TimeOnly(EPOCH);
		TimeOnly target = new TimeOnly(EPOCH + 1000L);
		assertSame(original, timeType.replace(original, target, null));
	}

	@Test
	void testTimeOnlyObjectToSQLStringNull() throws Exception {
		assertEquals("NULL", timeType.objectToSQLString(null, null));
	}

	@Test
	void testTimeOnlyObjectToSQLStringNonNull() throws Exception {
		java.util.Date d = new TimeOnly(EPOCH);
		String result = timeType.objectToSQLString(d, null);
		assertTrue(result.startsWith("'"));
		assertTrue(result.endsWith("'"));
	}

	@Test
	void testTimeOnlyNullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getTime("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(Boolean.TRUE);
		assertNull(timeType.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	void testTimeOnlyNullSafeGetReturnsTimeOnlyWhenNotNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		Time t = new Time(EPOCH);
		when(rs.getTime("col")).thenReturn(t);
		when(rs.wasNull()).thenReturn(Boolean.FALSE);
		Object result = timeType.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertTrue(result instanceof TimeOnly);
		// TimeOnly strips date component and millis
		assertEquals(new TimeOnly(EPOCH).getTime(), ((java.util.Date) result).getTime());
	}

	@Test
	void testTimeOnlyNullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		timeType.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.TIME);
	}

	@Test
	void testTimeOnlyNullSafeSetNonNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		TimeOnly t = new TimeOnly(EPOCH);
		timeType.nullSafeSet(ps, t, 2, null);
		// TimeOnly strips date component and millis, must match what UserType actually passes
		verify(ps).setTime(2, new Time(t.getTime()));
	}

	// ======== DateOnlyUserType ========

	private final DateOnlyUserType dateType = new DateOnlyUserType();

	@Test
	void testDateOnlySqlTypes() {
		assertArrayEquals(new int[] {Types.DATE}, dateType.sqlTypes());
	}

	@Test
	void testDateOnlyReturnedClass() {
		assertEquals(DateOnly.class, dateType.returnedClass());
	}

	@Test
	void testDateOnlyIsMutable() {
		assertTrue(dateType.isMutable());
	}

	@Test
	void testDateOnlyEqualsBothNull() {
		assertTrue(dateType.equals(null, null));
	}

	@Test
	void testDateOnlyEqualsXNull() {
		assertFalse(dateType.equals(null, new DateOnly(EPOCH)));
	}

	@Test
	void testDateOnlyEqualsYNull() {
		assertFalse(dateType.equals(new DateOnly(EPOCH), null));
	}

	@Test
	void testDateOnlyEqualsSameEpoch() {
		assertTrue(dateType.equals(new DateOnly(EPOCH), new DateOnly(EPOCH)));
	}

	@Test
	void testDateOnlyEqualsDistinctDatesDiffer() {
		long tomorrow = EPOCH + 86400000L;
		assertFalse(dateType.equals(new DateOnly(EPOCH), new DateOnly(tomorrow)));
	}

	@Test
	void testDateOnlyHashCodeConsistent() {
		DateOnly d1 = new DateOnly(EPOCH);
		DateOnly d2 = new DateOnly(EPOCH);
		assertEquals(dateType.hashCode(d1), dateType.hashCode(d2));
	}

	@Test
	void testDateOnlyDeepCopyNull() {
		assertNull(dateType.deepCopy(null));
	}

	@Test
	void testDateOnlyDeepCopyNonNull() {
		DateOnly d = new DateOnly(EPOCH);
		Object copy = dateType.deepCopy(d);
		assertTrue(copy instanceof DateOnly);
		// DateOnly strips time component, compare via the domain object's own getTime()
		assertEquals(d.getTime(), ((java.util.Date) copy).getTime());
		assertNotSame(d, copy, "deep copy must not be same reference");
	}

	@Test
	void testDateOnlyDisassemble() {
		DateOnly d = new DateOnly(EPOCH);
		assertSame(d, dateType.disassemble(d));
	}

	@Test
	void testDateOnlyAssemble() {
		DateOnly d = new DateOnly(EPOCH);
		assertSame(d, dateType.assemble(d, null));
	}

	@Test
	void testDateOnlyReplace() {
		DateOnly original = new DateOnly(EPOCH);
		DateOnly target = new DateOnly(EPOCH + 86400000L);
		assertSame(original, dateType.replace(original, target, null));
	}

	@Test
	void testDateOnlyObjectToSQLStringNull() throws Exception {
		assertEquals("NULL", dateType.objectToSQLString(null, null));
	}

	@Test
	void testDateOnlyObjectToSQLStringNonNull() throws Exception {
		Date d = new Date(EPOCH);
		String result = dateType.objectToSQLString(d, null);
		assertTrue(result.startsWith("'"));
		assertTrue(result.endsWith("'"));
	}

	@Test
	void testDateOnlyNullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getDate("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(Boolean.TRUE);
		assertNull(dateType.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	void testDateOnlyNullSafeGetReturnsDateOnlyWhenNotNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		Date d = new Date(EPOCH);
		when(rs.getDate("col")).thenReturn(d);
		when(rs.wasNull()).thenReturn(Boolean.FALSE);
		Object result = dateType.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertTrue(result instanceof DateOnly);
		// DateOnly strips time component, compare via what a new DateOnly(EPOCH) would produce
		assertEquals(new DateOnly(EPOCH).getTime(), ((java.util.Date) result).getTime());
	}

	@Test
	void testDateOnlyNullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		dateType.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.DATE);
	}

	@Test
	void testDateOnlyNullSafeSetNonNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		DateOnly d = new DateOnly(EPOCH);
		dateType.nullSafeSet(ps, d, 2, null);
		// DateOnly strips time component; verify with the actual stripped time value
		verify(ps).setDate(2, new Date(d.getTime()));
	}

	// ======== DateTimeUserType ========

	private final DateTimeUserType dtType = new DateTimeUserType();

	@Test
	void testDateTimeSqlTypes() {
		assertArrayEquals(new int[] {Types.TIMESTAMP}, dtType.sqlTypes());
	}

	@Test
	void testDateTimeReturnedClass() {
		assertEquals(DateTime.class, dtType.returnedClass());
	}

	@Test
	void testDateTimeIsMutable() {
		assertTrue(dtType.isMutable());
	}

	@Test
	void testDateTimeEqualsBothNull() {
		assertTrue(dtType.equals(null, null));
	}

	@Test
	void testDateTimeEqualsSameEpoch() {
		assertTrue(dtType.equals(new DateTime(EPOCH), new DateTime(EPOCH)));
	}

	@Test
	void testDateTimeEqualsXNull() {
		assertFalse(dtType.equals(null, new DateTime(EPOCH)));
	}

	@Test
	void testDateTimeEqualsYNull() {
		assertFalse(dtType.equals(new DateTime(EPOCH), null));
	}

	@Test
	void testDateTimeDifferentEpochNotEqual() {
		assertFalse(dtType.equals(new DateTime(EPOCH), new DateTime(EPOCH + 1000L)));
	}

	@Test
	void testDateTimeEqualsSqlTimestampsWithSameNanos() {
		// Both java.sql.Timestamp instances with same getTime() → exercises L50-54
		// Setting nanos to same sub-milli fraction: millis part of nanos = EPOCH % 1000 = 123,
		// so setNanos(123_000_050) keeps getTime() the same and gives xn = yn = 50 → equal
		Timestamp ts1 = new Timestamp(EPOCH);
		ts1.setNanos(123000050); // millis=123, sub-millis=50ns
		Timestamp ts2 = new Timestamp(EPOCH);
		ts2.setNanos(123000050); // same sub-millis fraction
		assertTrue(dtType.equals(ts1, ts2));
	}

        @Test
        void testDateTimeEqualsSqlTimestampsWithDifferentNanos() {
                // Both Timestamps with same getTime() but different sub-milli nanos → exercises L54 false branch
                Timestamp ts1 = new Timestamp(EPOCH);
                ts1.setNanos(123000050);
                Timestamp ts2 = new Timestamp(EPOCH);
                ts2.setNanos(123000075); // different sub-millis → xn != yn
                assertFalse(dtType.equals(ts1, ts2));
        }

	@Test
	void testDateTimeDeepCopyNonNull() {
		DateTime d = new DateTime(EPOCH);
		Object copy = dtType.deepCopy(d);
		assertTrue(copy instanceof DateTime);
		// DateTime strips millis, compare via domain object's own getTime()
		assertEquals(d.getTime(), ((java.util.Date) copy).getTime());
		assertNotSame(d, copy, "deep copy must not be same reference");
	}

	@Test
	void testDateTimeDisassemble() {
		DateTime d = new DateTime(EPOCH);
		assertSame(d, dtType.disassemble(d));
	}

	@Test
	void testDateTimeAssemble() {
		DateTime d = new DateTime(EPOCH);
		assertSame(d, dtType.assemble(d, null));
	}

	@Test
	void testDateTimeReplace() {
		DateTime original = new DateTime(EPOCH);
		DateTime target = new DateTime(EPOCH + 1000L);
		assertSame(original, dtType.replace(original, target, null));
	}

	@Test
	void testDateTimeObjectToSQLStringNull() throws Exception {
		assertEquals("NULL", dtType.objectToSQLString(null, null));
	}

	@Test
	void testDateTimeObjectToSQLStringNonNull() throws Exception {
		java.util.Date d = new DateTime(EPOCH);
		String result = dtType.objectToSQLString(d, null);
		assertTrue(result.startsWith("'"));
		assertTrue(result.endsWith("'"));
	}

	@Test
	void testDateTimeNullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getTimestamp("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(Boolean.TRUE);
		assertNull(dtType.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	void testDateTimeNullSafeGetReturnsDateTimeWhenNotNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getTimestamp("col")).thenReturn(new Timestamp(EPOCH));
		when(rs.wasNull()).thenReturn(Boolean.FALSE);
		Object result = dtType.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertTrue(result instanceof DateTime);
		// DateTime strips millis
		assertEquals(new DateTime(EPOCH).getTime(), ((java.util.Date) result).getTime());
	}

	@Test
	void testDateTimeNullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		dtType.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.TIMESTAMP);
	}

	@Test
	void testDateTimeNullSafeSetNonNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		DateTime d = new DateTime(EPOCH);
		dtType.nullSafeSet(ps, d, 2, null);
		// DateTime strips millis; verify with the actual stripped time value
		verify(ps).setTimestamp(2, new Timestamp(d.getTime()));
	}

	// ======== TimestampUserType ========

	private final TimestampUserType tsType = new TimestampUserType();

	@Test
	void testTimestampSqlTypes() {
		assertArrayEquals(new int[] {Types.TIMESTAMP}, tsType.sqlTypes());
	}

	@Test
	void testTimestampReturnedClass() {
		assertEquals(org.skyve.domain.types.Timestamp.class, tsType.returnedClass());
	}

	@Test
	void testTimestampIsMutable() {
		assertTrue(tsType.isMutable());
	}

	@Test
	void testTimestampEqualsBothNull() {
		assertTrue(tsType.equals(null, null));
	}

	@Test
	void testTimestampEqualsSameEpoch() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertTrue(tsType.equals(t, new org.skyve.domain.types.Timestamp(EPOCH)));
	}

	@Test
	void testTimestampEqualsDifferentEpochNotEqual() {
		org.skyve.domain.types.Timestamp t1 = new org.skyve.domain.types.Timestamp(EPOCH);
		// add 2 seconds so the difference survives the millis-clear in the constructor
		org.skyve.domain.types.Timestamp t2 = new org.skyve.domain.types.Timestamp(EPOCH + 2000L);
		assertFalse(tsType.equals(t1, t2));
	}

	@Test
	void testTimestampDeepCopyNull() {
		assertNull(tsType.deepCopy(null));
	}

	@Test
	void testTimestampDeepCopyNonNull() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		Object copy = tsType.deepCopy(t);
		assertTrue(copy instanceof org.skyve.domain.types.Timestamp);
		// Timestamp strips millis, compare via domain object's own getTime()
		assertEquals(t.getTime(), ((java.util.Date) copy).getTime());
		assertNotSame(t, copy, "deep copy must not be same reference");
	}

	@Test
	void testTimestampNullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getTimestamp("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(Boolean.TRUE);
		assertNull(tsType.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	void testTimestampNullSafeGetReturnsTimestampWhenNotNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getTimestamp("col")).thenReturn(new Timestamp(EPOCH));
		when(rs.wasNull()).thenReturn(Boolean.FALSE);
		Object result = tsType.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertTrue(result instanceof org.skyve.domain.types.Timestamp);
		// Timestamp strips millis
		assertEquals(new org.skyve.domain.types.Timestamp(EPOCH).getTime(), ((java.util.Date) result).getTime());
	}

	@Test
	void testTimestampNullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		tsType.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.TIMESTAMP);
	}

	@Test
	void testTimestampNullSafeSetNonNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		tsType.nullSafeSet(ps, t, 2, null);
		// Timestamp strips millis; verify with the actual stripped time value
		verify(ps).setTimestamp(2, new Timestamp(t.getTime()));
	}

	@Test
	void testTimestampObjectToSQLStringNull() throws Exception {
		assertEquals("NULL", tsType.objectToSQLString(null, null));
	}

	@Test
	void testTimestampObjectToSQLStringNonNull() throws Exception {
		java.util.Date d = new org.skyve.domain.types.Timestamp(EPOCH);
		String result = tsType.objectToSQLString(d, null);
		assertTrue(result.startsWith("'"));
		assertTrue(result.endsWith("'"));
	}

	@Test
	void testTimestampEqualsOneNull() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertFalse(tsType.equals(null, t));
		assertFalse(tsType.equals(t, null));
	}

	@Test
	void testTimestampEqualsSameRef() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertTrue(tsType.equals(t, t));
	}

	@Test
	void testTimestampEqualsJavaSqlTimestampSameNanos() {
		// Use a base time rounded to a full second so setNanos(sub-ms value)
		// does not change the ms component returned by getTime().
		long baseTime = (EPOCH / 1000L) * 1000L;
		Timestamp ts1 = new Timestamp(baseTime);
		ts1.setNanos(300); // 300 ns, ms part unchanged
		Timestamp ts2 = new Timestamp(baseTime);
		ts2.setNanos(300); // same nanos → xn == yn → equal
		assertTrue(tsType.equals(ts1, ts2));
	}

	@Test
	void testTimestampEqualsJavaSqlTimestampDifferentNanos() {
		long baseTime = (EPOCH / 1000L) * 1000L;
		Timestamp ts1 = new Timestamp(baseTime);
		ts1.setNanos(100); // 100 ns
		Timestamp ts2 = new Timestamp(baseTime);
		ts2.setNanos(200); // 200 ns → xn=100 != yn=200 → not equal
		assertFalse(tsType.equals(ts1, ts2));
	}

	@Test
	void testTimestampDisassembleReturnsValue() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertSame(t, tsType.disassemble(t));
	}

	@Test
	void testTimestampAssembleReturnsCached() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertSame(t, tsType.assemble(t, null));
	}

	@Test
	void testTimestampReplaceReturnsOriginal() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertSame(t, tsType.replace(t, null, null));
	}

	@Test
	void testTimestampHashCode() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertEquals(Long.valueOf(t.getTime() / 1000).hashCode(), tsType.hashCode(t));
	}

        @Test
        void testDateTimeHashCode() {
                DateTime d = new DateTime(EPOCH);
                assertEquals(Long.valueOf(d.getTime() / 1000).hashCode(), dtType.hashCode(d));
        }

        @Test
        void testDateTimeDeepCopyNull() {
                assertNull(dtType.deepCopy(null));
        }
}
