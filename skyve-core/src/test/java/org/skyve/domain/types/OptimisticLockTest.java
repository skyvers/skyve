package org.skyve.domain.types;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Date;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;

@SuppressWarnings("java:S8692") // system clock OK
class OptimisticLockTest {
	@Test
	@SuppressWarnings("static-method")
	void testUTCConversion() {
		Date date = new Date();
		OptimisticLock lock = new OptimisticLock("test", date);
		Assertions.assertEquals(date, lock.getTimestamp());

		lock = new OptimisticLock(lock.toString());
		Assertions.assertEquals(date, lock.getTimestamp());

		Assertions.assertEquals(date.toString(), lock.getTimestamp().toString());
	}

	@Test
	@SuppressWarnings("static-method")
	void testNonExistentLocalTimeConversion() {
		OptimisticLock lock = new OptimisticLock("20191006022000121admin");
		Assertions.assertEquals("20191006022000121admin", lock.toString());
	}

	@Test
	@SuppressWarnings("static-method")
	void getUsernameReturnsCorrectValue() {
		OptimisticLock lock = new OptimisticLock("mike", new Date());
		assertEquals("mike", lock.getUsername());
	}

	@Test
	@SuppressWarnings("static-method")
	void setUsernameUpdatesValue() {
		OptimisticLock lock = new OptimisticLock("mike", new Date());
		lock.setUsername("admin");
		assertEquals("admin", lock.getUsername());
	}

	@Test
	@SuppressWarnings("static-method")
	void setTimestampUpdatesValue() {
		Date original = new Date(1000L);
		Date updated = new Date(2000L);
		OptimisticLock lock = new OptimisticLock("user", original);
		lock.setTimestamp(updated);
		assertEquals(updated, lock.getTimestamp());
	}

	@Test
	@SuppressWarnings("static-method")
	void equalsReturnsTrueForSameValues() {
		Date date = new Date(0);
		OptimisticLock a = new OptimisticLock("user", date);
		OptimisticLock b = new OptimisticLock("user", date);
		assertEquals(a, b);
	}

	@Test
	@SuppressWarnings("static-method")
	void equalsReturnsFalseForDifferentUsername() {
		Date date = new Date(0);
		OptimisticLock a = new OptimisticLock("alice", date);
		OptimisticLock b = new OptimisticLock("bob", date);
		assertNotEquals(a, b);
	}

	@Test
	@SuppressWarnings("static-method")
	void equalsReturnsFalseForNonOptimisticLock() {
		OptimisticLock lock = new OptimisticLock("user", new Date());
		assertNotEquals(lock, "notALock");
	}

	@Test
	@SuppressWarnings("static-method")
	void hashCodeIsConsistentForEqualObjects() {
		Date date = new Date(0);
		OptimisticLock a = new OptimisticLock("user", date);
		OptimisticLock b = new OptimisticLock("user", date);
		assertEquals(a.hashCode(), b.hashCode());
	}

	@Test
	@SuppressWarnings("static-method")
	void hashCodeDiffersForDifferentUsers() {
		Date date = new Date(0);
		OptimisticLock a = new OptimisticLock("alice", date);
		OptimisticLock b = new OptimisticLock("bob", date);
		assertNotEquals(a.hashCode(), b.hashCode());
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorWithNullStringThrowsDomainException() {
		assertThrows(DomainException.class, () -> new OptimisticLock((String) null));
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorWithShortStringThrowsDomainException() {
		assertThrows(DomainException.class, () -> new OptimisticLock("tooshort"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toStringProducesRoundTrippableLockString() {
		Date date = new Date(0);
		OptimisticLock lock = new OptimisticLock("admin", date);
		String lockStr = lock.toString();
		assertNotNull(lockStr);
		OptimisticLock roundTripped = new OptimisticLock(lockStr);
		assertEquals("admin", roundTripped.getUsername());
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorWithUnparsableDateThrowsDomainException() {
		// 17 chars where the first 17 form an invalid date (month 13), so ParseException is caught → DomainException
		assertThrows(DomainException.class, () -> new OptimisticLock("19991301000000000admin"));
	}
}
