package org.skyve.job;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.Date;
import java.util.List;

import org.junit.Test;
import org.skyve.domain.Bean;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.user.User;
import org.skyve.web.BackgroundTask;

@SuppressWarnings("static-method")
public class JobSchedulerTest {
	private static class RecordingScheduler implements JobScheduler {
		private boolean preRestoreCalled;
		private boolean runRestoreJobCalled;
		private Bean restoreBean;

		@Override
		public void preRestore() {
			preRestoreCalled = true;
		}

		@Override
		public void runRestoreJob(Bean restoreOptions) {
			runRestoreJobCalled = true;
			restoreBean = restoreOptions;
		}

		@Override
		public void startup() {
			// no-op
		}

		@Override
		public void shutdown() {
			// no-op
		}

		@Override
		public void runOneShotJob(JobMetaData job, Bean parameter, User user) {
			// no-op
		}

		@Override
		public void runOneShotJob(JobMetaData job, Bean parameter, User user, int sleepAtEndInSeconds) {
			// no-op
		}

		@Override
		public <T extends Bean> void runBackgroundTask(Class<? extends BackgroundTask<T>> taskClass, User user, String webId) {
			// no-op
		}

		@Override
		public void scheduleOneShotJob(JobMetaData job, Bean parameter, User user, Date when) {
			// no-op
		}

		@Override
		public void scheduleJob(JobSchedule jobSchedule, User user) {
			// no-op
		}

		@Override
		public void unscheduleJob(String uuid, String customerName) {
			// no-op
		}

		@Override
		public void scheduleReport(JobSchedule reportSchedule, User user) {
			// no-op
		}

		@Override
		public void unscheduleReport(String uuid, String customerName) {
			// no-op
		}

		@Override
		public List<JobDescription> getCustomerRunningJobs() {
			return List.of();
		}

		@Override
		public boolean cancelJob(String instanceId) {
			return false;
		}

		@Override
		public void postRestore(boolean restoreSuccessful) {
			// no-op
		}

		@Override
		public void validateMetaData() {
			// no-op
		}

		@Override
		public void runContentGarbageCollector() {
			// no-op
		}
	}

	@Test
	public void defaultRestoreCallsPreRestoreAndRunRestoreJob() {
		RecordingScheduler scheduler = new RecordingScheduler();
		Bean restoreOptions = mock(Bean.class);

		scheduler.restore(restoreOptions);

		assertTrue(scheduler.preRestoreCalled);
		assertTrue(scheduler.runRestoreJobCalled);
		assertThat(scheduler.restoreBean, is(restoreOptions));
	}
}
