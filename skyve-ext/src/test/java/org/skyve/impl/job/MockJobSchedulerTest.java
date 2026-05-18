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
		scheduler().startup();
	}

	@Test
	public void testShutdown() {
		scheduler().shutdown();
	}

	@Test
	public void testRunOneShotJobNoSleep() {
		scheduler().runOneShotJob(null, null, null);
	}

	@Test
	public void testRunOneShotJobWithSleep() {
		scheduler().runOneShotJob(null, null, null, 0);
	}

	@Test
	public void testRunBackgroundTask() {
		scheduler().runBackgroundTask(null, null, null);
	}

	@Test
	public void testRunContentGarbageCollector() {
		scheduler().runContentGarbageCollector();
	}

	@Test
	public void testScheduleOneShotJob() {
		scheduler().scheduleOneShotJob(null, null, null, null);
	}

	@Test
	public void testScheduleJob() {
		scheduler().scheduleJob(null, null);
	}

	@Test
	public void testUnscheduleJob() {
		scheduler().unscheduleJob("uuid-1", "demo");
	}

	@Test
	public void testScheduleReport() {
		scheduler().scheduleReport(null, null);
	}

	@Test
	public void testUnscheduleReport() {
		scheduler().unscheduleReport("uuid-1", "demo");
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
		scheduler().validateMetaData();
	}

	@Test
	public void testPreRestore() {
		scheduler().preRestore();
	}

	@Test
	public void testRunRestoreJob() {
		scheduler().runRestoreJob(null);
	}

	@Test
	public void testPostRestore() {
		scheduler().postRestore(true);
		scheduler().postRestore(false);
	}
}
