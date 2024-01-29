package org.skyve.impl.cdi;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.job.JobDescription;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.user.User;
import org.skyve.web.BackgroundTask;

import jakarta.enterprise.inject.Alternative;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class JobSchedulerInjectable implements JobScheduler, Serializable {
	private static final long serialVersionUID = 7634387574414670560L;

	@Override
	public void startup() {
		EXT.getJobScheduler().startup();
	}

	@Override
	public void shutdown() {
		EXT.getJobScheduler().shutdown();
	}

	@Override
	public void addReportJob(String reportName) {
		EXT.getJobScheduler().addReportJob(reportName);
	}

	@Override
	public void runOneShotJob(JobMetaData job, Bean parameter, User user) {
		EXT.getJobScheduler().runOneShotJob(job, parameter, user);
	}

	@Override
	public void runOneShotJob(JobMetaData job, Bean parameter, User user, int sleepAtEndInSeconds) {
		EXT.getJobScheduler().runOneShotJob(job, parameter, user, sleepAtEndInSeconds);
	}

	@Override
	public <T extends Bean> void runBackgroundTask(Class<? extends BackgroundTask<T>> taskClass, User user, String webId) {
		EXT.getJobScheduler().runBackgroundTask(taskClass, user, webId);
	}

	@Override
	public void runContentGarbageCollector() {
		EXT.getJobScheduler().runContentGarbageCollector();
	}
	
	@Override
	public void scheduleOneShotJob(JobMetaData job, Bean parameter, User user, Date when) {
		EXT.getJobScheduler().scheduleOneShotJob(job, parameter, user, when);
	}

	@Override
	public void scheduleJob(Bean jobSchedule, User user) {
		EXT.getJobScheduler().scheduleJob(jobSchedule, user);
	}

	@Override
	public void unscheduleJob(Bean jobSchedule, Customer customer) {
		EXT.getJobScheduler().unscheduleJob(jobSchedule, customer);
	}

	@Override
	public void scheduleReport(Bean reportSchedule, User user) {
		EXT.getJobScheduler().scheduleReport(reportSchedule, user);
	}

	@Override
	public void unscheduleReport(Bean reportSchedule, Customer customer) {
		EXT.getJobScheduler().unscheduleReport(reportSchedule, customer);
	}

	@Override
	public List<JobDescription> getCustomerRunningJobs() {
		return EXT.getJobScheduler().getCustomerRunningJobs();
	}

	@Override
	public boolean cancelJob(String instanceId) {
		return EXT.getJobScheduler().cancelJob(instanceId);
	}
}
