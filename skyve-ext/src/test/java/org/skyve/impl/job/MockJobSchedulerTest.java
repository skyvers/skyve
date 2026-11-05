package org.skyve.impl.job;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.Date;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.job.JobSchedule;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.user.User;
import org.skyve.web.BackgroundTask;

/**
 * Verifies that {@link MockJobScheduler} can be constructed and that all
 * no-op methods complete without throwing, and that methods with return
 * values return the expected stubs.
 */
@SuppressWarnings("static-method")
class MockJobSchedulerTest {

	private static MockJobScheduler scheduler() {
		return new MockJobScheduler();
	}

	private static JobMetaData job() {
		return mock(JobMetaData.class);
	}

	private static User user() {
		return mock(User.class);
	}

	private static JobSchedule jobSchedule() {
		return mock(JobSchedule.class);
	}

	@Test
	void testStartup() {
		MockJobScheduler s = scheduler();
		s.startup();
		assertNotNull(s);
	}

	@Test
	void testShutdown() {
		MockJobScheduler s = scheduler();
		s.shutdown();
		assertNotNull(s);
	}

	@Test
	void testRunOneShotJobNoSleep() {
		MockJobScheduler s = scheduler();
		s.runOneShotJob(job(), null, user());
		assertNotNull(s);
	}

	@Test
	void testRunOneShotJobWithSleep() {
		MockJobScheduler s = scheduler();
		s.runOneShotJob(job(), null, user(), 0);
		assertNotNull(s);
	}

	@Test
	void testRunBackgroundTask() {
		MockJobScheduler s = scheduler();
		s.runBackgroundTask(TestBackgroundTask.class, user(), "webId");
		assertNotNull(s);
	}

	@Test
	void testRunContentGarbageCollector() {
		MockJobScheduler s = scheduler();
		s.runContentGarbageCollector();
		assertNotNull(s);
	}

	@Test
	void testScheduleOneShotJob() {
		MockJobScheduler s = scheduler();
		s.scheduleOneShotJob(job(), null, user(), new Date(0L));
		assertNotNull(s);
	}

	@Test
	void testScheduleJob() {
		MockJobScheduler s = scheduler();
		s.scheduleJob(jobSchedule(), user());
		assertNotNull(s);
	}

	@Test
	void testUnscheduleJob() {
		MockJobScheduler s = scheduler();
		s.unscheduleJob("uuid-1", "demo");
		assertNotNull(s);
	}

	@Test
	void testScheduleReport() {
		MockJobScheduler s = scheduler();
		s.scheduleReport(jobSchedule(), user());
		assertNotNull(s);
	}

	@Test
	void testUnscheduleReport() {
		MockJobScheduler s = scheduler();
		s.unscheduleReport("uuid-1", "demo");
		assertNotNull(s);
	}

	@Test
	void testGetCustomerRunningJobsReturnsEmptyList() {
		assertNotNull(scheduler().getCustomerRunningJobs());
		assertTrue(scheduler().getCustomerRunningJobs().isEmpty());
	}

	@Test
	void testCancelJobReturnsTrue() {
		assertTrue(scheduler().cancelJob("instance-1"));
	}

	@Test
	void testValidateMetaData() {
		MockJobScheduler s = scheduler();
		s.validateMetaData();
		assertNotNull(s);
	}

	@Test
	void testPreRestore() {
		MockJobScheduler s = scheduler();
		s.preRestore();
		assertNotNull(s);
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void testRunRestoreJob() {
		MockJobScheduler s = scheduler();
		s.runRestoreJob(null);
		assertNotNull(s);
	}

	@Test
	void testPostRestore() {
		MockJobScheduler s = scheduler();
		s.postRestore(true);
		s.postRestore(false);
		assertNotNull(s);
	}

	private static final class TestBackgroundTask implements BackgroundTask<Bean> {
		@Override
		public Bean getBean() {
			return mock(Bean.class);
		}

		@Override
		public void cacheConversationAndCycleTransaction() {
			// no-op
		}

		@Override
		public void execute(Bean bean) {
			// no-op
		}
	}
}
