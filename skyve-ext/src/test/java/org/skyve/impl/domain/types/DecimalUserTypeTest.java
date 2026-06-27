package org.skyve.impl.domain.types;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Types;

import org.junit.Test;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;

@SuppressWarnings({"resource", "boxing"})
public class DecimalUserTypeTest {

	// ======== Decimal2UserType ========

	private final Decimal2UserType dec2 = new Decimal2UserType();

	@Test
	public void testDec2ReturnedClass() {
		assertEquals(Decimal2.class, dec2.returnedClass());
	}

	@Test
	public void testDec2SqlTypes() {
		assertArrayEquals(new int[] {Types.NUMERIC}, dec2.sqlTypes());
	}

	@Test
	public void testDec2IsMutableFalse() {
		assertFalse(dec2.isMutable());
	}

	@Test
	public void testDec2EqualsNullNull() {
		assertTrue(dec2.equals(null, null));
	}

	@Test
	public void testDec2EqualsNullValue() {
		assertFalse(dec2.equals(null, new Decimal2("1.23")));
	}

	@Test
	public void testDec2EqualsSameRef() {
		Decimal2 d = new Decimal2("5.00");
		assertTrue(dec2.equals(d, d));
	}

	@Test
	public void testDec2EqualsSameValue() {
		assertTrue(dec2.equals(new Decimal2("3.14"), new Decimal2("3.14")));
	}

	@Test
	public void testDec2NotEqual() {
		assertFalse(dec2.equals(new Decimal2("1.00"), new Decimal2("2.00")));
	}

	@Test
	public void testDec2HashCode() {
		Decimal2 d = new Decimal2("99.99");
		assertEquals(d.intValue(), dec2.hashCode(d));
	}

	@Test
	public void testDec2DeepCopyNull() {
		assertNull(dec2.deepCopy(null));
	}

	@Test
	public void testDec2DeepCopyNonNull() {
		Decimal2 original = new Decimal2("7.77");
		Object copy = dec2.deepCopy(original);
		assertEquals(original, copy);
		assertNotSame("deep copy must not be same reference", original, copy);
	}

	@Test
	public void testDec2Disassemble() {
		Decimal2 d = new Decimal2("1.00");
		assertSame(d, dec2.disassemble(d));
	}

	@Test
	public void testDec2Assemble() {
		Decimal2 d = new Decimal2("2.00");
		assertSame(d, dec2.assemble(d, null));
	}

	@Test
	public void testDec2Replace() {
		Decimal2 original = new Decimal2("3.00");
		Decimal2 target = new Decimal2("4.00");
		assertSame(original, dec2.replace(original, target, null));
	}

	@Test
	public void testDec2ObjectToSQLStringNull() throws Exception {
		assertEquals("NULL", dec2.objectToSQLString(null, null));
	}

	@Test
	public void testDec2ObjectToSQLStringNonNull() throws Exception {
		Decimal2 d = new Decimal2("12.34");
		assertEquals(d.bigDecimalValue().toString(), dec2.objectToSQLString(d, null));
	}

	@Test
	public void testDec2NullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getBigDecimal("val")).thenReturn(null);
		when(rs.wasNull()).thenReturn(Boolean.TRUE);
		assertNull(dec2.nullSafeGet(rs, new String[] {"val"}, null, null));
	}

	@Test
	public void testDec2NullSafeGetReturnsDecimalWhenNotNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getBigDecimal("val")).thenReturn(new BigDecimal("2.22"));
		when(rs.wasNull()).thenReturn(Boolean.FALSE);
		Object result = dec2.nullSafeGet(rs, new String[] {"val"}, null, null);
		assertEquals(new Decimal2("2.22"), result);
	}

	@Test
	public void testDec2NullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		dec2.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.NUMERIC);
	}

	@Test
	public void testDec2NullSafeSetDecimal2() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		Decimal2 d = new Decimal2("5.55");
		dec2.nullSafeSet(ps, d, 2, null);
		verify(ps).setBigDecimal(2, d.bigDecimalValue());
	}

	@Test
	public void testDec2NullSafeSetDecimal() throws Exception {
		// covers L63-64: value instanceof Decimal (but not Decimal2)
		PreparedStatement ps = mock(PreparedStatement.class);
		Decimal5 d = new Decimal5("3.33333");
		dec2.nullSafeSet(ps, d, 1, null);
		verify(ps).setBigDecimal(1, new Decimal2(d).bigDecimalValue());
	}

	@Test
	public void testDec2NullSafeSetBigDecimal() throws Exception {
		// covers L66-67: value instanceof BigDecimal
		PreparedStatement ps = mock(PreparedStatement.class);
		BigDecimal d = new BigDecimal("7.77");
		dec2.nullSafeSet(ps, d, 1, null);
		verify(ps).setBigDecimal(1, new Decimal2(d).bigDecimalValue());
	}

	@Test
	public void testDec2NullSafeSetDoubleNumber() throws Exception {
		// covers L70: else Number path
		PreparedStatement ps = mock(PreparedStatement.class);
		Double d = Double.valueOf(2.22);
		dec2.nullSafeSet(ps, d, 1, null);
		verify(ps).setBigDecimal(1, new Decimal2(d.doubleValue()).bigDecimalValue());
	}

	// ======== Decimal5UserType ========

	private final Decimal5UserType dec5 = new Decimal5UserType();

	@Test
	public void testDec5ReturnedClass() {
		assertEquals(Decimal5.class, dec5.returnedClass());
	}

	@Test
	public void testDec5IsMutableFalse() {
		assertFalse(dec5.isMutable());
	}

	@Test
	public void testDec5DeepCopyNull() {
		assertNull(dec5.deepCopy(null));
	}

	@Test
	public void testDec5DeepCopyNonNull() {
		Decimal5 original = new Decimal5("3.33333");
		Object copy = dec5.deepCopy(original);
		assertEquals(original, copy);
	}

	@Test
	public void testDec5NullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getBigDecimal("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(Boolean.TRUE);
		assertNull(dec5.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	public void testDec5NullSafeGetReturnsDecimal5WhenNotNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getBigDecimal("col")).thenReturn(new BigDecimal("1.23456"));
		when(rs.wasNull()).thenReturn(Boolean.FALSE);
		Object result = dec5.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertEquals(new Decimal5("1.23456"), result);
	}

	@Test
	public void testDec5NullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		dec5.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.NUMERIC);
	}

	@Test
	public void testDec5NullSafeSetDecimal5() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		Decimal5 d = new Decimal5("9.99999");
		dec5.nullSafeSet(ps, d, 1, null);
		verify(ps).setBigDecimal(1, d.bigDecimalValue());
	}

	// ======== Decimal10UserType ========

	private final Decimal10UserType dec10 = new Decimal10UserType();

	@Test
	public void testDec10ReturnedClass() {
		assertEquals(Decimal10.class, dec10.returnedClass());
	}

	@Test
	public void testDec10IsMutableFalse() {
		assertFalse(dec10.isMutable());
	}

	@Test
	public void testDec10DeepCopyNull() {
		assertNull(dec10.deepCopy(null));
	}

	@Test
	public void testDec10DeepCopyNonNull() {
		Decimal10 original = new Decimal10("1234567890.12");
		Object copy = dec10.deepCopy(original);
		assertEquals(original, copy);
	}

	@Test
	public void testDec10NullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getBigDecimal("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(Boolean.TRUE);
		assertNull(dec10.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	public void testDec10NullSafeGetReturnsDecimal10WhenNotNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getBigDecimal("col")).thenReturn(new BigDecimal("9999999.1234567890"));
		when(rs.wasNull()).thenReturn(Boolean.FALSE);
		Object result = dec10.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertEquals(new Decimal10("9999999.1234567890"), result);
	}

	@Test
	public void testDec10NullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		dec10.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.NUMERIC);
	}

	@Test
	public void testDec10NullSafeSetDecimal10() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		Decimal10 d = new Decimal10("1.1234567890");
		dec10.nullSafeSet(ps, d, 1, null);
		verify(ps).setBigDecimal(1, d.bigDecimalValue());
	}

	@Test
	public void testDec10NullSafeSetBigDecimal() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		BigDecimal d = new BigDecimal("3.14");
		dec10.nullSafeSet(ps, d, 1, null);
		verify(ps).setBigDecimal(1, new Decimal10(d).bigDecimalValue());
	}

	@Test
	public void testDec10NullSafeSetDecimal2() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		Decimal2 d = new Decimal2("2.22");
		dec10.nullSafeSet(ps, d, 1, null);
		verify(ps).setBigDecimal(1, new Decimal10(d).bigDecimalValue());
	}

	@Test
	public void testDec10ObjectToSQLStringNull() throws Exception {
		assertEquals("NULL", dec10.objectToSQLString(null, null));
	}

	@Test
	public void testDec10ObjectToSQLStringNonNull() throws Exception {
		Decimal10 d = new Decimal10("42.0");
		assertEquals(d.toString(), dec10.objectToSQLString(d, null));
	}

	@Test
	public void testDec10SqlTypes() {
		assertArrayEquals(new int[] {Types.NUMERIC}, dec10.sqlTypes());
	}

	@Test
	public void testDec10EqualsEqualValues() {
		Decimal10 a = new Decimal10("1.0000000001");
		Decimal10 b = new Decimal10("1.0000000001");
		assertTrue(dec10.equals(a, b));
	}

	@Test
	public void testDec10HashCode() {
		Decimal10 d = new Decimal10("1.0");
		assertEquals(d.intValue(), dec10.hashCode(d));
	}

	@Test
	public void testDec10NullSafeSetDoubleNumber() throws Exception {
		// covers L70: the Number else path
		PreparedStatement ps = mock(PreparedStatement.class);
		Double d = Double.valueOf(3.14);
		dec10.nullSafeSet(ps, d, 1, null);
		verify(ps).setBigDecimal(1, new Decimal10(d.doubleValue()).bigDecimalValue());
	}

	@Test
	public void testDec10DisassembleReturnsValue() {
		Decimal10 d = new Decimal10("1.1234567890");
		assertSame(d, dec10.disassemble(d));
	}

	@Test
	public void testDec10AssembleReturnsCached() {
		Decimal10 d = new Decimal10("9.8765432100");
		assertSame(d, dec10.assemble(d, null));
	}

	@Test
	public void testDec10ReplaceReturnsOriginal() {
		Decimal10 original = new Decimal10("4.5678901234");
		assertSame(original, dec10.replace(original, null, null));
	}

        @Test
        public void testDec10EqualsNullNull() {
                assertTrue(dec10.equals(null, null));
        }

        @Test
        public void testDec10EqualsNullValue() {
                assertFalse(dec10.equals(null, new Decimal10("1.0")));
        }

        @Test
        public void testDec10EqualsSameRef() {
                Decimal10 d = new Decimal10("5.0");
                assertTrue(dec10.equals(d, d));
        }

        @Test
        public void testDec10EqualsUnequalValues() {
                assertFalse(dec10.equals(new Decimal10("1.0"), new Decimal10("2.0")));
        }

        // ======== Decimal5UserType — missing branches ========

        @Test
        public void testDec5SqlTypes() {
                assertArrayEquals(new int[] {Types.NUMERIC}, dec5.sqlTypes());
        }

	@Test
	public void testDec5EqualsNullNull() {
		assertTrue(dec5.equals(null, null));
	}

	@Test
	public void testDec5EqualsNullValue() {
		assertFalse(dec5.equals(null, new Decimal5("1.23456")));
	}

	@Test
	public void testDec5EqualsSameRef() {
		Decimal5 d = new Decimal5("5.00000");
		assertTrue(dec5.equals(d, d));
	}

	@Test
	public void testDec5EqualsSameValue() {
		assertTrue(dec5.equals(new Decimal5("3.14159"), new Decimal5("3.14159")));
	}

	@Test
	public void testDec5NotEqual() {
		assertFalse(dec5.equals(new Decimal5("1.00000"), new Decimal5("2.00000")));
	}

	@Test
	public void testDec5HashCode() {
		Decimal5 d = new Decimal5("99.99999");
		assertEquals(d.intValue(), dec5.hashCode(d));
	}

	@Test
	public void testDec5NullSafeSetDecimal2() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		Decimal2 d = new Decimal2("2.22");
		dec5.nullSafeSet(ps, d, 1, null);
		verify(ps).setBigDecimal(1, new Decimal5(d).bigDecimalValue());
	}

	@Test
	public void testDec5NullSafeSetBigDecimal() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		BigDecimal d = new BigDecimal("3.14159");
		dec5.nullSafeSet(ps, d, 1, null);
		verify(ps).setBigDecimal(1, new Decimal5(d).bigDecimalValue());
	}

	@Test
	public void testDec5NullSafeSetNumber() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		Double d = Double.valueOf(2.71828);
		dec5.nullSafeSet(ps, d, 1, null);
		verify(ps).setBigDecimal(1, new Decimal5(d.doubleValue()).bigDecimalValue());
	}

	@Test
	public void testDec5DisassembleReturnsValue() {
		Decimal5 d = new Decimal5("1.23456");
		assertSame(d, dec5.disassemble(d));
	}

	@Test
	public void testDec5AssembleReturnsCached() {
		Decimal5 d = new Decimal5("9.87654");
		assertSame(d, dec5.assemble(d, null));
	}

	@Test
	public void testDec5ReplaceReturnsOriginal() {
		Decimal5 original = new Decimal5("4.56789");
		assertSame(original, dec5.replace(original, null, null));
	}

	@Test
	public void testDec5ObjectToSQLStringNull() throws Exception {
		assertEquals("NULL", dec5.objectToSQLString(null, null));
	}

	@Test
	public void testDec5ObjectToSQLStringNonNull() throws Exception {
		Decimal5 d = new Decimal5("42.00000");
		assertEquals(d.toString(), dec5.objectToSQLString(d, null));
	}
}
