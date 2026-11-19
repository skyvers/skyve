package org.skyve.impl.job;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.job.JobDescription;
import org.skyve.job.JobSchedule;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.user.User;
import org.skyve.web.BackgroundTask;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A no-op Job Scheduler to use in tests.
 */
public class MockJobScheduler implements JobScheduler {
	public MockJobScheduler() {
		// nothing to see here
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
	public void runOneShotJob(@Nonnull JobMetaData job, @Nullable Bean parameter, @Nonnull User user) {
		// no-op
	}

	@Override
	public void runOneShotJob(@Nonnull JobMetaData job, @Nullable Bean parameter, @Nonnull User user, int sleepAtEndInSeconds) {
		// no-op
	}
	
	@Override
	public <T extends Bean> void runBackgroundTask(@Nonnull Class<? extends BackgroundTask<T>> taskClass, @Nonnull User user, @Nonnull String webId) {
		// no-op
	}
	
	@Override
	public void runContentGarbageCollector() {
		// no-op
	}

	@Override
	public void scheduleOneShotJob(@Nonnull JobMetaData job, @Nullable Bean parameter, @Nonnull User user, @Nonnull Date when) {
		// no-op
	}
	
	@Override
	public void scheduleJob(@Nonnull JobSchedule jobSchedule, @Nonnull User user) {
		// no-op
	}
	
	@Override
	public void unscheduleJob(@Nonnull String uuid, @Nonnull String customerName) {
		// no-op
	}

	@Override
	public void scheduleReport(@Nonnull JobSchedule reportSchedule, @Nonnull User user) {
		// no-op
	}

	@Override
	public void unscheduleReport(@Nonnull String uuid, @Nonnull String customerName) {
		// no-op
	}

	@Override
	public @Nonnull List<JobDescription> getCustomerRunningJobs() {
		return Collections.emptyList();
	}

	@Override
	public boolean cancelJob(@Nonnull String instanceId) {
		return true;
	}
	
	@Override
	public void validateMetaData() {
		// no-op
	}

	@Override
	public void preRestore() {
		// no-op
	}
	
	@Override
	public void runRestoreJob(@Nonnull Bean restoreOptions) {
		// no-op
	}
	
	@Override
	public void postRestore(boolean restoreSuccessful) {
		// no-op
	}
}
