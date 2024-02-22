package org.skyve.domain.types;

import java.util.Date;

import org.junit.Assert;
import org.junit.jupiter.api.Test;

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
}
