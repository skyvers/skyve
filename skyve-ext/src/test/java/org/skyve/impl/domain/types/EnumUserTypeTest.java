package org.skyve.impl.domain.types;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Types;
import java.util.Properties;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;

@SuppressWarnings({"resource", "boxing"})
class EnumUserTypeTest {

	private EnumUserType type;

	@BeforeEach
	void setUp() {
		type = new EnumUserType();
		Properties props = new Properties();
		props.setProperty("enumClass", DatasetType.class.getName());
		type.setParameterValues(props);
	}

	@Test
	void testSqlTypes() {
		assertArrayEquals(new int[] {Types.VARCHAR}, type.sqlTypes());
	}

	@Test
	void testReturnedClassIsEnumClass() {
		assertEquals(DatasetType.class, type.returnedClass());
	}

	@Test
	void testIsMutableFalse() {
		assertFalse(type.isMutable());
	}

	@Test
	void testEqualsBothNull() {
		assertTrue(type.equals(null, null));
	}

	@Test
	void testEqualsSameRef() {
		DatasetType d = DatasetType.bizQL;
		assertTrue(type.equals(d, d));
	}

	@Test
	void testEqualsSameValue() {
		assertTrue(type.equals(DatasetType.SQL, DatasetType.SQL));
	}

	@Test
	void testEqualsXNull() {
		assertFalse(type.equals(null, DatasetType.SQL));
	}

	@Test
	void testEqualsDistinctValues() {
		assertFalse(type.equals(DatasetType.bizQL, DatasetType.SQL));
	}

	@Test
	void testHashCode() {
		DatasetType d = DatasetType.bizQL;
		assertEquals(d.hashCode(), type.hashCode(d));
	}

	@Test
	void testDeepCopyReturnsIdentity() {
		DatasetType d = DatasetType.constant;
		assertSame(d, type.deepCopy(d));
	}

	@Test
	void testDisassemble() {
		DatasetType d = DatasetType.classValue;
		assertSame(d, type.disassemble(d));
	}

	@Test
	void testAssemble() {
		DatasetType d = DatasetType.SQL;
		assertSame(d, type.assemble(d, null));
	}

	@Test
	void testReplace() {
		DatasetType original = DatasetType.bizQL;
		DatasetType target = DatasetType.SQL;
		assertSame(original, type.replace(original, target, null));
	}

	@Test
	void testNullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(Boolean.TRUE);
		assertNull(type.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	void testNullSafeGetReturnsNullForEmptyString() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn("");
		when(rs.wasNull()).thenReturn(Boolean.FALSE);
		assertNull(type.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	void testNullSafeGetReturnsEnumValueByCode() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn("BizQL");
		when(rs.wasNull()).thenReturn(Boolean.FALSE);
		Object result = type.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertEquals(DatasetType.bizQL, result);
	}

	@Test
	void testNullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		type.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.VARCHAR);
	}

	@Test
	void testNullSafeSetStringValue() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		type.nullSafeSet(ps, "SQL", 1, null);
		verify(ps).setString(1, "SQL");
	}

	@Test
	void testNullSafeSetEnumValue() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		type.nullSafeSet(ps, DatasetType.constant, 2, null);
		verify(ps).setString(2, DatasetType.constant.toCode());
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetParameterValuesClassNotFound() {
		// covers L31-32: ClassNotFoundException catch block
		EnumUserType newType = new EnumUserType();
		Properties props = new Properties();
		props.setProperty("enumClass", "com.nonexistent.DoesNotExist");
		assertThrows(org.hibernate.HibernateException.class, () -> newType.setParameterValues(props));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetParameterValuesNoToCodeMethod() {
		// covers L42-43: Exception catch block in readResolve() when toCode method absent
		// java.lang.Thread$State is a real enum with no toCode() method
		EnumUserType newType = new EnumUserType();
		Properties props = new Properties();
		props.setProperty("enumClass", "java.lang.Thread$State");
		assertThrows(org.hibernate.HibernateException.class, () -> newType.setParameterValues(props));
	}

	/**
	 * Helper enum: has toCode() but no fromCode(String) — triggers the L49-50 catch block.
	 */
	public enum ToCodeOnlyEnum {
		X;
		public static String toCode() {
			return "X";
		}
		// deliberately no fromCode(String code) method
	}

	@Test
	@SuppressWarnings("static-method")
	void testSetParameterValuesNoFromCodeMethodThrows() {
		// covers L49-50: fromCode method not found → HibernateException
		EnumUserType newType = new EnumUserType();
		Properties props = new Properties();
		props.setProperty("enumClass", ToCodeOnlyEnum.class.getName());
		assertThrows(org.hibernate.HibernateException.class, () -> newType.setParameterValues(props));
	}

	/**
	 * Helper enum: fromCode(String) throws at runtime — triggers L99-100 catch in nullSafeGet.
	 */
	public enum ThrowingFromCodeEnum {
		X;
		public static String toCode() {
			return "X";
		}
		@SuppressWarnings("unused")
		public static ThrowingFromCodeEnum fromCode(String code) {
			throw new RuntimeException("fromCode-test-failure");
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testNullSafeGetFromCodeInvocationThrowsHibernateException() throws Exception {
		// covers L99-100: fromCode method invocation throws → HibernateException
		EnumUserType newType = new EnumUserType();
		Properties props = new Properties();
		props.setProperty("enumClass", ThrowingFromCodeEnum.class.getName());
		newType.setParameterValues(props);
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn("X");
		assertThrows(org.hibernate.HibernateException.class, () -> newType.nullSafeGet(rs, new String[]{"col"}, null, null));
	}

	/**
	 * Helper enum: toCode() throws at runtime — triggers L120-121 catch in nullSafeSet.
	 */
	public enum ThrowingToCodeEnum {
		X;
		public static String toCode() {
			throw new RuntimeException("toCode-test-failure");
		}
		@SuppressWarnings("unused")
		public static ThrowingToCodeEnum fromCode(String code) {
			return X;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testNullSafeSetToCodeInvocationThrowsHibernateException() {
		// covers L120-121: toCode method invocation throws → HibernateException
		EnumUserType newType = new EnumUserType();
		Properties props = new Properties();
		props.setProperty("enumClass", ThrowingToCodeEnum.class.getName());
		newType.setParameterValues(props);
		PreparedStatement ps = mock(PreparedStatement.class);
		// pass a non-String, non-null value so the else-branch invokes toCode()
		assertThrows(org.hibernate.HibernateException.class, () -> newType.nullSafeSet(ps, ThrowingToCodeEnum.X, 1, null));
	}
}
