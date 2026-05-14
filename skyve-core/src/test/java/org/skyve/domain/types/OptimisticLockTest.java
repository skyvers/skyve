package org.skyve.domain.types;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Date;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;

public class OptimisticLockTest {
	@Test
	@SuppressWarnings("static-method")
	public void testUTCConversion() {
		Date date = new Date();
		OptimisticLock lock = new OptimisticLock("test", date);
		Assert.assertEquals(date, lock.getTimestamp());

		lock = new OptimisticLock(lock.toString());
		Assert.assertEquals(date, lock.getTimestamp());

		Assert.assertEquals(date.toString(), lock.getTimestamp().toString());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testNonExistentLocalTimeConversion() {
		OptimisticLock lock = new OptimisticLock("20191006022000121admin");
		Assert.assertEquals("20191006022000121admin", lock.toString());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getUsernameReturnsCorrectValue() {
		OptimisticLock lock = new OptimisticLock("mike", new Date());
		assertEquals("mike", lock.getUsername());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setUsernameUpdatesValue() {
		OptimisticLock lock = new OptimisticLock("mike", new Date());
		lock.setUsername("admin");
		assertEquals("admin", lock.getUsername());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setTimestampUpdatesValue() {
		Date original = new Date(1000L);
		Date updated = new Date(2000L);
		OptimisticLock lock = new OptimisticLock("user", original);
		lock.setTimestamp(updated);
		assertEquals(updated, lock.getTimestamp());
	}

	@Test
	@SuppressWarnings("static-method")
	public void equalsReturnsTrueForSameValues() {
		Date date = new Date(0);
		OptimisticLock a = new OptimisticLock("user", date);
		OptimisticLock b = new OptimisticLock("user", date);
		assertTrue(a.equals(b));
	}

	@Test
	@SuppressWarnings("static-method")
	public void equalsReturnsFalseForDifferentUsername() {
		Date date = new Date(0);
		OptimisticLock a = new OptimisticLock("alice", date);
		OptimisticLock b = new OptimisticLock("bob", date);
		assertFalse(a.equals(b));
	}

	@Test
	@SuppressWarnings({ "static-method", "unlikely-arg-type" })
	public void equalsReturnsFalseForNonOptimisticLock() {
		OptimisticLock lock = new OptimisticLock("user", new Date());
		assertFalse(lock.equals("notALock"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void hashCodeIsConsistentForEqualObjects() {
		Date date = new Date(0);
		OptimisticLock a = new OptimisticLock("user", date);
		OptimisticLock b = new OptimisticLock("user", date);
		assertEquals(a.hashCode(), b.hashCode());
	}

	@Test
	@SuppressWarnings("static-method")
	public void hashCodeDiffersForDifferentUsers() {
		Date date = new Date(0);
		OptimisticLock a = new OptimisticLock("alice", date);
		OptimisticLock b = new OptimisticLock("bob", date);
		assertNotEquals(a.hashCode(), b.hashCode());
	}

	@Test
	@SuppressWarnings("static-method")
	public void constructorWithNullStringThrowsDomainException() {
		assertThrows(DomainException.class, () -> new OptimisticLock((String) null));
	}

	@Test
	@SuppressWarnings("static-method")
	public void constructorWithShortStringThrowsDomainException() {
		assertThrows(DomainException.class, () -> new OptimisticLock("tooshort"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toStringProducesRoundTrippableLockString() {
		Date date = new Date(0);
		OptimisticLock lock = new OptimisticLock("admin", date);
		String lockStr = lock.toString();
		assertNotNull(lockStr);
		OptimisticLock roundTripped = new OptimisticLock(lockStr);
		assertEquals("admin", roundTripped.getUsername());
	}
}
