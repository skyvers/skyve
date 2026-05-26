package org.skyve.impl.job;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Verifies that {@link MockJobScheduler} can be constructed and that all
 * no-op methods complete without throwing, and that methods with return
 * values return the expected stubs.
 */
@SuppressWarnings("static-method")
public class MockJobSchedulerTest {

	private static MockJobScheduler scheduler() {
		return new MockJobScheduler();
	}

	@Test
	public void testStartup() {
		MockJobScheduler s = scheduler();
		s.startup();
		assertNotNull(s);
	}

	@Test
	public void testShutdown() {
		MockJobScheduler s = scheduler();
		s.shutdown();
		assertNotNull(s);
	}

	@Test
	public void testRunOneShotJobNoSleep() {
		MockJobScheduler s = scheduler();
		s.runOneShotJob(null, null, null);
		assertNotNull(s);
	}

	@Test
	public void testRunOneShotJobWithSleep() {
		MockJobScheduler s = scheduler();
		s.runOneShotJob(null, null, null, 0);
		assertNotNull(s);
	}

	@Test
	public void testRunBackgroundTask() {
		MockJobScheduler s = scheduler();
		s.runBackgroundTask(null, null, null);
		assertNotNull(s);
	}

	@Test
	public void testRunContentGarbageCollector() {
		MockJobScheduler s = scheduler();
		s.runContentGarbageCollector();
		assertNotNull(s);
	}

	@Test
	public void testScheduleOneShotJob() {
		MockJobScheduler s = scheduler();
		s.scheduleOneShotJob(null, null, null, null);
		assertNotNull(s);
	}

	@Test
	public void testScheduleJob() {
		MockJobScheduler s = scheduler();
		s.scheduleJob(null, null);
		assertNotNull(s);
	}

	@Test
	public void testUnscheduleJob() {
		MockJobScheduler s = scheduler();
		s.unscheduleJob("uuid-1", "demo");
		assertNotNull(s);
	}

	@Test
	public void testScheduleReport() {
		MockJobScheduler s = scheduler();
		s.scheduleReport(null, null);
		assertNotNull(s);
	}

	@Test
	public void testUnscheduleReport() {
		MockJobScheduler s = scheduler();
		s.unscheduleReport("uuid-1", "demo");
		assertNotNull(s);
	}

	@Test
	public void testGetCustomerRunningJobsReturnsEmptyList() {
		assertNotNull(scheduler().getCustomerRunningJobs());
		assertTrue(scheduler().getCustomerRunningJobs().isEmpty());
	}

	@Test
	public void testCancelJobReturnsTrue() {
		assertTrue(scheduler().cancelJob("instance-1"));
	}

	@Test
	public void testValidateMetaData() {
		MockJobScheduler s = scheduler();
		s.validateMetaData();
		assertNotNull(s);
	}

	@Test
	public void testPreRestore() {
		MockJobScheduler s = scheduler();
		s.preRestore();
		assertNotNull(s);
	}

	@Test
	public void testRunRestoreJob() {
		MockJobScheduler s = scheduler();
		s.runRestoreJob(null);
		assertNotNull(s);
	}

	@Test
	public void testPostRestore() {
		MockJobScheduler s = scheduler();
		s.postRestore(true);
		s.postRestore(false);
		assertNotNull(s);
	}
}
