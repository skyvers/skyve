package org.skyve.impl.domain.types;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Time;
import java.sql.Timestamp;
import java.sql.Types;

import org.junit.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;

@SuppressWarnings({"resource", "boxing"})
public class DateTimeUserTypeTest {

	// A fixed epoch value for tests — 2024-06-15 10:30:45.123 UTC
	private static final long EPOCH = 1718447445123L;

	// ======== TimeOnlyUserType ========

	private final TimeOnlyUserType timeType = new TimeOnlyUserType();

	@Test
	public void testTimeOnlySqlTypes() {
		assertArrayEquals(new int[] {Types.TIME}, timeType.sqlTypes());
	}

	@Test
	public void testTimeOnlyReturnedClass() {
		assertEquals(TimeOnly.class, timeType.returnedClass());
	}

	@Test
	public void testTimeOnlyIsMutable() {
		assertTrue(timeType.isMutable());
	}

	@Test
	public void testTimeOnlyEqualsBothNull() {
		// x == y (both null → same ref)
		assertTrue(timeType.equals(null, null));
	}

	@Test
	public void testTimeOnlyEqualsXNull() {
		assertFalse(timeType.equals(null, new TimeOnly(EPOCH)));
	}

	@Test
	public void testTimeOnlyEqualsYNull() {
		assertFalse(timeType.equals(new TimeOnly(EPOCH), null));
	}

	@Test
	public void testTimeOnlyEqualsSameEpoch() {
		assertTrue(timeType.equals(new TimeOnly(EPOCH), new TimeOnly(EPOCH)));
	}

	@Test
	public void testTimeOnlyEqualsDistinctTimesDiffer() {
		// Different millis = different time-of-day
		long later = EPOCH + 3600000L; // +1 hour
		assertFalse(timeType.equals(new TimeOnly(EPOCH), new TimeOnly(later)));
	}

	@Test
	public void testTimeOnlyHashCodeConsistent() {
		TimeOnly t1 = new TimeOnly(EPOCH);
		TimeOnly t2 = new TimeOnly(EPOCH);
		assertEquals(timeType.hashCode(t1), timeType.hashCode(t2));
	}

	@Test
	public void testTimeOnlyDeepCopyNull() {
		assertNull(timeType.deepCopy(null));
	}

	@Test
	public void testTimeOnlyDeepCopyNonNull() {
		TimeOnly t = new TimeOnly(EPOCH);
		Object copy = timeType.deepCopy(t);
		// TimeOnly strips date-component and millis, compare via its own getTime()
		assertEquals(t.getTime(), ((java.util.Date) copy).getTime());
		assertFalse("deep copy must not be same reference", copy == t);
	}

	@Test
	public void testTimeOnlyDisassemble() {
		TimeOnly t = new TimeOnly(EPOCH);
		assertSame(t, timeType.disassemble(t));
	}

	@Test
	public void testTimeOnlyAssemble() {
		TimeOnly t = new TimeOnly(EPOCH);
		assertSame(t, timeType.assemble(t, null));
	}

	@Test
	public void testTimeOnlyReplace() {
		TimeOnly original = new TimeOnly(EPOCH);
		TimeOnly target = new TimeOnly(EPOCH + 1000L);
		assertSame(original, timeType.replace(original, target, null));
	}

	@Test
	public void testTimeOnlyObjectToSQLStringNull() throws Exception {
		assertEquals("NULL", timeType.objectToSQLString(null, null));
	}

	@Test
	public void testTimeOnlyObjectToSQLStringNonNull() throws Exception {
		java.util.Date d = new TimeOnly(EPOCH);
		String result = timeType.objectToSQLString(d, null);
		assertTrue(result.startsWith("'"));
		assertTrue(result.endsWith("'"));
	}

	@Test
	public void testTimeOnlyNullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getTime("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(true);
		assertNull(timeType.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	public void testTimeOnlyNullSafeGetReturnsTimeOnlyWhenNotNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		Time t = new Time(EPOCH);
		when(rs.getTime("col")).thenReturn(t);
		when(rs.wasNull()).thenReturn(false);
		Object result = timeType.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertTrue(result instanceof TimeOnly);
		// TimeOnly strips date component and millis
		assertEquals(new TimeOnly(EPOCH).getTime(), ((java.util.Date) result).getTime());
	}

	@Test
	public void testTimeOnlyNullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		timeType.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.TIME);
	}

	@Test
	public void testTimeOnlyNullSafeSetNonNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		TimeOnly t = new TimeOnly(EPOCH);
		timeType.nullSafeSet(ps, t, 2, null);
		// TimeOnly strips date component and millis, must match what UserType actually passes
		verify(ps).setTime(2, new Time(t.getTime()));
	}

	// ======== DateOnlyUserType ========

	private final DateOnlyUserType dateType = new DateOnlyUserType();

	@Test
	public void testDateOnlySqlTypes() {
		assertArrayEquals(new int[] {Types.DATE}, dateType.sqlTypes());
	}

	@Test
	public void testDateOnlyReturnedClass() {
		assertEquals(DateOnly.class, dateType.returnedClass());
	}

	@Test
	public void testDateOnlyIsMutable() {
		assertTrue(dateType.isMutable());
	}

	@Test
	public void testDateOnlyEqualsBothNull() {
		assertTrue(dateType.equals(null, null));
	}

	@Test
	public void testDateOnlyEqualsXNull() {
		assertFalse(dateType.equals(null, new DateOnly(EPOCH)));
	}

	@Test
	public void testDateOnlyEqualsYNull() {
		assertFalse(dateType.equals(new DateOnly(EPOCH), null));
	}

	@Test
	public void testDateOnlyEqualsSameEpoch() {
		assertTrue(dateType.equals(new DateOnly(EPOCH), new DateOnly(EPOCH)));
	}

	@Test
	public void testDateOnlyEqualsDistinctDatesDiffer() {
		long tomorrow = EPOCH + 86400000L;
		assertFalse(dateType.equals(new DateOnly(EPOCH), new DateOnly(tomorrow)));
	}

	@Test
	public void testDateOnlyHashCodeConsistent() {
		DateOnly d1 = new DateOnly(EPOCH);
		DateOnly d2 = new DateOnly(EPOCH);
		assertEquals(dateType.hashCode(d1), dateType.hashCode(d2));
	}

	@Test
	public void testDateOnlyDeepCopyNull() {
		assertNull(dateType.deepCopy(null));
	}

	@Test
	public void testDateOnlyDeepCopyNonNull() {
		DateOnly d = new DateOnly(EPOCH);
		Object copy = dateType.deepCopy(d);
		assertTrue(copy instanceof DateOnly);
		// DateOnly strips time component, compare via the domain object's own getTime()
		assertEquals(d.getTime(), ((java.util.Date) copy).getTime());
		assertFalse("deep copy must not be same reference", copy == d);
	}

	@Test
	public void testDateOnlyDisassemble() {
		DateOnly d = new DateOnly(EPOCH);
		assertSame(d, dateType.disassemble(d));
	}

	@Test
	public void testDateOnlyAssemble() {
		DateOnly d = new DateOnly(EPOCH);
		assertSame(d, dateType.assemble(d, null));
	}

	@Test
	public void testDateOnlyReplace() {
		DateOnly original = new DateOnly(EPOCH);
		DateOnly target = new DateOnly(EPOCH + 86400000L);
		assertSame(original, dateType.replace(original, target, null));
	}

	@Test
	public void testDateOnlyObjectToSQLStringNull() throws Exception {
		assertEquals("NULL", dateType.objectToSQLString(null, null));
	}

	@Test
	public void testDateOnlyObjectToSQLStringNonNull() throws Exception {
		Date d = new Date(EPOCH);
		String result = dateType.objectToSQLString(d, null);
		assertTrue(result.startsWith("'"));
		assertTrue(result.endsWith("'"));
	}

	@Test
	public void testDateOnlyNullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getDate("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(true);
		assertNull(dateType.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	public void testDateOnlyNullSafeGetReturnsDateOnlyWhenNotNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		Date d = new Date(EPOCH);
		when(rs.getDate("col")).thenReturn(d);
		when(rs.wasNull()).thenReturn(false);
		Object result = dateType.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertTrue(result instanceof DateOnly);
		// DateOnly strips time component, compare via what a new DateOnly(EPOCH) would produce
		assertEquals(new DateOnly(EPOCH).getTime(), ((java.util.Date) result).getTime());
	}

	@Test
	public void testDateOnlyNullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		dateType.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.DATE);
	}

	@Test
	public void testDateOnlyNullSafeSetNonNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		DateOnly d = new DateOnly(EPOCH);
		dateType.nullSafeSet(ps, d, 2, null);
		// DateOnly strips time component; verify with the actual stripped time value
		verify(ps).setDate(2, new Date(d.getTime()));
	}

	// ======== DateTimeUserType ========

	private final DateTimeUserType dtType = new DateTimeUserType();

	@Test
	public void testDateTimeSqlTypes() {
		assertArrayEquals(new int[] {Types.TIMESTAMP}, dtType.sqlTypes());
	}

	@Test
	public void testDateTimeReturnedClass() {
		assertEquals(DateTime.class, dtType.returnedClass());
	}

	@Test
	public void testDateTimeIsMutable() {
		assertTrue(dtType.isMutable());
	}

	@Test
	public void testDateTimeEqualsBothNull() {
		assertTrue(dtType.equals(null, null));
	}

	@Test
	public void testDateTimeEqualsSameEpoch() {
		assertTrue(dtType.equals(new DateTime(EPOCH), new DateTime(EPOCH)));
	}

	@Test
	public void testDateTimeEqualsXNull() {
		assertFalse(dtType.equals(null, new DateTime(EPOCH)));
	}

	@Test
	public void testDateTimeEqualsYNull() {
		assertFalse(dtType.equals(new DateTime(EPOCH), null));
	}

	@Test
	public void testDateTimeDifferentEpochNotEqual() {
		assertFalse(dtType.equals(new DateTime(EPOCH), new DateTime(EPOCH + 1000L)));
	}

	@Test
	public void testDateTimeHashCodeConsistent() {
		DateTime d1 = new DateTime(EPOCH);
		DateTime d2 = new DateTime(EPOCH);
		assertEquals(dtType.hashCode(d1), dtType.hashCode(d2));
	}

	@Test
	public void testDateTimeDeepCopyNull() {
		assertNull(dtType.deepCopy(null));
	}

	@Test
	public void testDateTimeDeepCopyNonNull() {
		DateTime d = new DateTime(EPOCH);
		Object copy = dtType.deepCopy(d);
		assertTrue(copy instanceof DateTime);
		// DateTime strips millis, compare via domain object's own getTime()
		assertEquals(d.getTime(), ((java.util.Date) copy).getTime());
		assertFalse("deep copy must not be same reference", copy == d);
	}

	@Test
	public void testDateTimeDisassemble() {
		DateTime d = new DateTime(EPOCH);
		assertSame(d, dtType.disassemble(d));
	}

	@Test
	public void testDateTimeAssemble() {
		DateTime d = new DateTime(EPOCH);
		assertSame(d, dtType.assemble(d, null));
	}

	@Test
	public void testDateTimeReplace() {
		DateTime original = new DateTime(EPOCH);
		DateTime target = new DateTime(EPOCH + 1000L);
		assertSame(original, dtType.replace(original, target, null));
	}

	@Test
	public void testDateTimeObjectToSQLStringNull() throws Exception {
		assertEquals("NULL", dtType.objectToSQLString(null, null));
	}

	@Test
	public void testDateTimeObjectToSQLStringNonNull() throws Exception {
		java.util.Date d = new DateTime(EPOCH);
		String result = dtType.objectToSQLString(d, null);
		assertTrue(result.startsWith("'"));
		assertTrue(result.endsWith("'"));
	}

	@Test
	public void testDateTimeNullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getTimestamp("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(true);
		assertNull(dtType.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	public void testDateTimeNullSafeGetReturnsDateTimeWhenNotNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getTimestamp("col")).thenReturn(new Timestamp(EPOCH));
		when(rs.wasNull()).thenReturn(false);
		Object result = dtType.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertTrue(result instanceof DateTime);
		// DateTime strips millis
		assertEquals(new DateTime(EPOCH).getTime(), ((java.util.Date) result).getTime());
	}

	@Test
	public void testDateTimeNullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		dtType.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.TIMESTAMP);
	}

	@Test
	public void testDateTimeNullSafeSetNonNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		DateTime d = new DateTime(EPOCH);
		dtType.nullSafeSet(ps, d, 2, null);
		// DateTime strips millis; verify with the actual stripped time value
		verify(ps).setTimestamp(2, new Timestamp(d.getTime()));
	}

	// ======== TimestampUserType ========

	private final TimestampUserType tsType = new TimestampUserType();

	@Test
	public void testTimestampSqlTypes() {
		assertArrayEquals(new int[] {Types.TIMESTAMP}, tsType.sqlTypes());
	}

	@Test
	public void testTimestampReturnedClass() {
		assertEquals(org.skyve.domain.types.Timestamp.class, tsType.returnedClass());
	}

	@Test
	public void testTimestampIsMutable() {
		assertTrue(tsType.isMutable());
	}

	@Test
	public void testTimestampEqualsBothNull() {
		assertTrue(tsType.equals(null, null));
	}

	@Test
	public void testTimestampEqualsSameEpoch() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertTrue(tsType.equals(t, new org.skyve.domain.types.Timestamp(EPOCH)));
	}

	@Test
	public void testTimestampEqualsDifferentEpochNotEqual() {
		org.skyve.domain.types.Timestamp t1 = new org.skyve.domain.types.Timestamp(EPOCH);
		// add 2 seconds so the difference survives the millis-clear in the constructor
		org.skyve.domain.types.Timestamp t2 = new org.skyve.domain.types.Timestamp(EPOCH + 2000L);
		assertFalse(tsType.equals(t1, t2));
	}

	@Test
	public void testTimestampDeepCopyNull() {
		assertNull(tsType.deepCopy(null));
	}

	@Test
	public void testTimestampDeepCopyNonNull() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		Object copy = tsType.deepCopy(t);
		assertTrue(copy instanceof org.skyve.domain.types.Timestamp);
		// Timestamp strips millis, compare via domain object's own getTime()
		assertEquals(t.getTime(), ((java.util.Date) copy).getTime());
		assertFalse("deep copy must not be same reference", copy == t);
	}

	@Test
	public void testTimestampNullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getTimestamp("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(true);
		assertNull(tsType.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	public void testTimestampNullSafeGetReturnsTimestampWhenNotNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getTimestamp("col")).thenReturn(new Timestamp(EPOCH));
		when(rs.wasNull()).thenReturn(false);
		Object result = tsType.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertTrue(result instanceof org.skyve.domain.types.Timestamp);
		// Timestamp strips millis
		assertEquals(new org.skyve.domain.types.Timestamp(EPOCH).getTime(), ((java.util.Date) result).getTime());
	}

	@Test
	public void testTimestampNullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		tsType.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.TIMESTAMP);
	}

	@Test
	public void testTimestampNullSafeSetNonNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		tsType.nullSafeSet(ps, t, 2, null);
		// Timestamp strips millis; verify with the actual stripped time value
		verify(ps).setTimestamp(2, new Timestamp(t.getTime()));
	}

	@Test
	public void testTimestampObjectToSQLStringNull() throws Exception {
		assertEquals("NULL", tsType.objectToSQLString(null, null));
	}

	@Test
	public void testTimestampObjectToSQLStringNonNull() throws Exception {
		java.util.Date d = new org.skyve.domain.types.Timestamp(EPOCH);
		String result = tsType.objectToSQLString(d, null);
		assertTrue(result.startsWith("'"));
		assertTrue(result.endsWith("'"));
	}

	@Test
	public void testTimestampEqualsOneNull() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertFalse(tsType.equals(null, t));
		assertFalse(tsType.equals(t, null));
	}

	@Test
	public void testTimestampEqualsSameRef() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertTrue(tsType.equals(t, t));
	}

	@Test
	public void testTimestampEqualsJavaSqlTimestampSameNanos() {
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
	public void testTimestampEqualsJavaSqlTimestampDifferentNanos() {
		long baseTime = (EPOCH / 1000L) * 1000L;
		Timestamp ts1 = new Timestamp(baseTime);
		ts1.setNanos(100); // 100 ns
		Timestamp ts2 = new Timestamp(baseTime);
		ts2.setNanos(200); // 200 ns → xn=100 != yn=200 → not equal
		assertFalse(tsType.equals(ts1, ts2));
	}

	@Test
	public void testTimestampDisassembleReturnsValue() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertSame(t, tsType.disassemble(t));
	}

	@Test
	public void testTimestampAssembleReturnsCached() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertSame(t, tsType.assemble(t, null));
	}

	@Test
	public void testTimestampReplaceReturnsOriginal() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertSame(t, tsType.replace(t, null, null));
	}

	@Test
	public void testTimestampHashCode() {
		org.skyve.domain.types.Timestamp t = new org.skyve.domain.types.Timestamp(EPOCH);
		assertEquals(Long.valueOf(t.getTime() / 1000).hashCode(), tsType.hashCode(t));
	}
}
