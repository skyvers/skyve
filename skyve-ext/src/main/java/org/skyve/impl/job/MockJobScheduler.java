package org.skyve.impl.job;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.job.JobDescription;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.user.User;
import org.skyve.web.BackgroundTask;

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
	public void runContentGarbageCollector() {
		// no-op
	}

	@Override
	public void scheduleOneShotJob(JobMetaData job, Bean parameter, User user, Date when) {
		// no-op
	}
	
	@Override
	public void scheduleJob(Bean jobSchedule, User user) {
		// no-op
	}
	
	@Override
	public void unscheduleJob(Bean jobSchedule, Customer customer) {
		// no-op
	}

	@Override
	public void scheduleReport(Bean reportSchedule, User user) {
		// no-op
	}

	@Override
	public void unscheduleReport(Bean reportSchedule, Customer customer) {
		// no-op
	}

	@Override
	public List<JobDescription> getCustomerRunningJobs() {
		return Collections.emptyList();
	}

	@Override
	public boolean cancelJob(String instanceId) {
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
	public void runRestoreJob(Bean restoreOptions) {
		// no-op
	}
	
	@Override
	public void postRestore(boolean restoreSuccessful) {
		// no-op
	}
}
