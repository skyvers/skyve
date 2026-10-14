package org.skyve.impl.job;

import org.skyve.job.JobScheduler;

import jakarta.annotation.Nonnull;

/**
 * A singleton for the Job Scheduler to use.
 * NB This doesn't need to be thread safe as it is set at startup only.
 */
public class JobSchedulerStaticSingleton {
	private static JobScheduler instance;
	
	private JobSchedulerStaticSingleton() {
		// nothing to see here
	}
	
	public static @Nonnull JobScheduler get() {
		return instance;
	}
	
	public static void set(@Nonnull JobScheduler instance) {
		JobSchedulerStaticSingleton.instance = instance;
	}

	public static void setDefault() {
		instance = new QuartzJobScheduler();
	}
}
