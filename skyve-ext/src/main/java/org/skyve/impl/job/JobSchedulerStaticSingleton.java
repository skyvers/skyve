package org.skyve.impl.job;

import org.skyve.job.JobScheduler;

/**
 * A singleton for the Job Scheduler to use.
 * NB This doesn't need to be thread safe as it is set at startup only.
 */
public class JobSchedulerStaticSingleton {
	private static JobScheduler instance;
	
	public static JobScheduler get() {
		return instance;
	}
	
	public static void set(JobScheduler instance) {
		JobSchedulerStaticSingleton.instance = instance;
	}

	public static void setDefault() {
		instance = new QuartzJobScheduler();
	}
}
