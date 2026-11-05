package org.skyve.impl.domain.types;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
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
import java.util.Date;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.OptimisticLock;

@SuppressWarnings({"resource", "boxing"})
class OptimisticLockUserTypeTest {

	private final OptimisticLockUserType type = new OptimisticLockUserType();

	private static OptimisticLock lock(String username) {
		return new OptimisticLock(username, new Date(1718447445000L));
	}

	@Test
	void testSqlTypes() {
		assertArrayEquals(new int[] {Types.VARCHAR}, type.sqlTypes());
	}

	@Test
	void testReturnedClass() {
		assertEquals(OptimisticLock.class, type.returnedClass());
	}

	@Test
	void testIsMutableTrue() {
		assertTrue(type.isMutable());
	}

	@Test
	void testEqualsBothNull() {
		assertTrue(type.equals(null, null));
	}

	@Test
	void testEqualsXNull() {
		assertFalse(type.equals(null, lock("admin")));
	}

	@Test
	void testEqualsYNull() {
		assertFalse(type.equals(lock("admin"), null));
	}

	@Test
	void testEqualsSameRef() {
		OptimisticLock l = lock("admin");
		assertTrue(type.equals(l, l));
	}

	@Test
	void testEqualsSameValue() {
		OptimisticLock l1 = lock("admin");
		OptimisticLock l2 = lock("admin");
		assertTrue(type.equals(l1, l2));
	}

	@Test
	void testEqualsDistinctUsers() {
		assertFalse(type.equals(lock("alice"), lock("bob")));
	}

	@Test
	void testHashCode() {
		OptimisticLock l = lock("admin");
		assertEquals(l.hashCode(), type.hashCode(l));
	}

	@Test
	void testDeepCopyNull() throws Exception {
		assertNull(type.deepCopy(null));
	}

	@Test
	void testDeepCopyNonNull() throws Exception {
		OptimisticLock original = lock("admin");
		Object copy = type.deepCopy(original);
		assertEquals(original, copy);
		assertNotSame(original, copy, "deep copy must not be same reference");
	}

	@Test
	void testDisassemble() {
		OptimisticLock l = lock("admin");
		assertSame(l, type.disassemble(l));
	}

	@Test
	void testAssemble() {
		OptimisticLock l = lock("admin");
		assertSame(l, type.assemble(l, null));
	}

	@Test
	void testReplace() {
		OptimisticLock original = lock("admin");
		OptimisticLock target = lock("other");
		assertSame(original, type.replace(original, target, null));
	}

	@Test
	void testObjectToSQLStringNull() throws Exception {
		assertEquals("NULL", type.objectToSQLString(null, null));
	}

	@Test
	void testObjectToSQLStringNonNull() throws Exception {
		OptimisticLock l = lock("admin");
		assertEquals(l.toString(), type.objectToSQLString(l, null));
	}

	@Test
	void testNullSafeGetReturnsNullWhenColumnNull() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn(null);
		when(rs.wasNull()).thenReturn(Boolean.TRUE);
		assertNull(type.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	void testNullSafeGetReturnsNullWhenEmptyString() throws Exception {
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn("");
		when(rs.wasNull()).thenReturn(Boolean.FALSE);
		assertNull(type.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	void testNullSafeGetReturnsLockWhenNotNull() throws Exception {
		OptimisticLock l = lock("admin");
		String lockString = l.toString();
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn(lockString);
		when(rs.wasNull()).thenReturn(Boolean.FALSE);
		Object result = type.nullSafeGet(rs, new String[] {"col"}, null, null);
		assertTrue(result instanceof OptimisticLock);
		assertEquals(l, result);
	}

	@Test
	void testNullSafeSetNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		type.nullSafeSet(ps, null, 1, null);
		verify(ps).setNull(1, Types.VARCHAR);
	}

	@Test
	void testNullSafeSetNonNull() throws Exception {
		PreparedStatement ps = mock(PreparedStatement.class);
		OptimisticLock l = lock("admin");
		type.nullSafeSet(ps, l, 2, null);
		verify(ps).setString(2, l.toString());
	}

	@Test
	void testNullSafeGetThrowsHibernateExceptionForMalformedValue() throws Exception {
		// covers L53-54: DomainException catch when lock string too short
		ResultSet rs = mock(ResultSet.class);
		when(rs.getString("col")).thenReturn("bad");
		when(rs.wasNull()).thenReturn(Boolean.FALSE);
		assertThrows(org.hibernate.HibernateException.class, () -> type.nullSafeGet(rs, new String[] {"col"}, null, null));
	}

	@Test
	void testDeepCopyThrowsHibernateExceptionForMalformedValue() {
		// covers L75-76: DomainException catch when toString() produces invalid lock string
		assertThrows(org.hibernate.HibernateException.class, () -> type.deepCopy("bad"));
	}
}
