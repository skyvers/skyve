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

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Types;
import java.util.Properties;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;

@SuppressWarnings({"resource", "boxing"})
public class EnumUserTypeTest {

	private EnumUserType type;

	@Before
	public void setUp() {
		type = new EnumUserType();
		Properties props = new Properties();
		props.setProperty("enumClass", DatasetType.class.getName());
		type.setParameterValues(props);
	}

	@Test
	public void testSqlTypes() {
		assertArrayEquals(new int[] {Types.VARCHAR}, type.sqlTypes());
	}

	@Test
	public void testReturnedClassIsEnumClass() {
		assertEquals(DatasetType.class, type.returnedClass());
	}

	@Test
	public void testIsMutableFalse() {
		assertFalse(type.isMutable());
	}

	@Test
	public void testEqualsBothNull() {
		assertTrue(type.equals(null, null));
	}

	@Test
	public void testEqualsSameRef() {
		DatasetType d = DatasetType.bizQL;
		assertTrue(type.equals(d, d));
	}

	@Test
	public void testEqualsSameValue() {
		assertTrue(type.equals(DatasetType.SQL, DatasetType.SQL));
	}

	@Test
	public void testEqualsXNull() {
		assertFalse(type.equals(null, DatasetType.SQL));
	}

	@Test
	public void testEqualsDistinctValues() {
		assertFalse(type.equals(DatasetType.bizQL, DatasetType.SQL));
	}

	@Test
	public void testHashCode() {
		DatasetType d = DatasetType.bizQL;
		assertEquals(d.hashCode(), type.hashCode(d));
	}

	@Test
	public void testDeepCopyReturnsIdentity() {
		DatasetType d = DatasetType.constant;
		assertSame(d, type.deepCopy(d));
	}

	@Test
	public void testDisassemble() {
		DatasetType d = DatasetType.classValue;
		assertSame(d, type.disassemble(d));
	}

	@Test
	public void testAssemble() {
		DatasetType d = DatasetType.SQL;
		assertSame(d, type.assemble(d, null));
	}

	@Test
	public void testReplace() {
		DatasetType original = DatasetType.bizQL;
		DatasetType target = DatasetType.SQL;
		assertSame(original, type.replace(original, target, null));
	}

	@Test
	public void testNullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(true);
		assertNull(type.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	public void testNullSafeGetReturnsNullForEmptyString() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn("");
		when(rs.wasNull()).thenReturn(false);
		assertNull(type.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	public void testNullSafeGetReturnsEnumValueByCode() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn("BizQL");
		when(rs.wasNull()).thenReturn(false);
		Object result = type.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertEquals(DatasetType.bizQL, result);
	}

	@Test
	public void testNullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		type.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.VARCHAR);
	}

	@Test
	public void testNullSafeSetStringValue() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		type.nullSafeSet(ps, "SQL", 1, null);
		verify(ps).setString(1, "SQL");
	}

	@Test
	public void testNullSafeSetEnumValue() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		type.nullSafeSet(ps, DatasetType.constant, 2, null);
		verify(ps).setString(2, DatasetType.constant.toCode());
	}
}
