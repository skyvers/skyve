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
import java.util.Date;

import org.junit.Test;
import org.skyve.domain.types.OptimisticLock;

@SuppressWarnings({"resource", "boxing"})
public class OptimisticLockUserTypeTest {

	private final OptimisticLockUserType type = new OptimisticLockUserType();

	private static OptimisticLock lock(String username) {
		return new OptimisticLock(username, new Date(1718447445000L));
	}

	@Test
	public void testSqlTypes() {
		assertArrayEquals(new int[] {Types.VARCHAR}, type.sqlTypes());
	}

	@Test
	public void testReturnedClass() {
		assertEquals(OptimisticLock.class, type.returnedClass());
	}

	@Test
	public void testIsMutableTrue() {
		assertTrue(type.isMutable());
	}

	@Test
	public void testEqualsBothNull() {
		assertTrue(type.equals(null, null));
	}

	@Test
	public void testEqualsXNull() {
		assertFalse(type.equals(null, lock("admin")));
	}

	@Test
	public void testEqualsYNull() {
		assertFalse(type.equals(lock("admin"), null));
	}

	@Test
	public void testEqualsSameRef() {
		OptimisticLock l = lock("admin");
		assertTrue(type.equals(l, l));
	}

	@Test
	public void testEqualsSameValue() {
		OptimisticLock l1 = lock("admin");
		OptimisticLock l2 = lock("admin");
		assertTrue(type.equals(l1, l2));
	}

	@Test
	public void testEqualsDistinctUsers() {
		assertFalse(type.equals(lock("alice"), lock("bob")));
	}

	@Test
	public void testHashCode() {
		OptimisticLock l = lock("admin");
		assertEquals(l.hashCode(), type.hashCode(l));
	}

	@Test
	public void testDeepCopyNull() throws Exception {
		assertNull(type.deepCopy(null));
	}

	@Test
	public void testDeepCopyNonNull() throws Exception {
		OptimisticLock original = lock("admin");
		Object copy = type.deepCopy(original);
		assertEquals(original, copy);
		assertFalse("deep copy must not be same reference", copy == original);
	}

	@Test
	public void testDisassemble() {
		OptimisticLock l = lock("admin");
		assertSame(l, type.disassemble(l));
	}

	@Test
	public void testAssemble() {
		OptimisticLock l = lock("admin");
		assertSame(l, type.assemble(l, null));
	}

	@Test
	public void testReplace() {
		OptimisticLock original = lock("admin");
		OptimisticLock target = lock("other");
		assertSame(original, type.replace(original, target, null));
	}

	@Test
	public void testObjectToSQLStringNull() throws Exception {
		assertEquals("NULL", type.objectToSQLString(null, null));
	}

	@Test
	public void testObjectToSQLStringNonNull() throws Exception {
		OptimisticLock l = lock("admin");
		assertEquals(l.toString(), type.objectToSQLString(l, null));
	}

	@Test
	public void testNullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(true);
		assertNull(type.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	public void testNullSafeGetReturnsNullWhenEmptyString() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn("");
		when(rs.wasNull()).thenReturn(false);
		assertNull(type.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	public void testNullSafeGetReturnsLockWhenNotNull() throws Exception {
		OptimisticLock l = lock("admin");
		String lockString = l.toString();
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn(lockString);
		when(rs.wasNull()).thenReturn(false);
		Object result = type.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertTrue(result instanceof OptimisticLock);
		assertEquals(l, result);
	}

	@Test
	public void testNullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		type.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.VARCHAR);
	}

	@Test
	public void testNullSafeSetNonNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		OptimisticLock l = lock("admin");
		type.nullSafeSet(ps, l, 2, null);
		verify(ps).setString(2, l.toString());
	}
}
