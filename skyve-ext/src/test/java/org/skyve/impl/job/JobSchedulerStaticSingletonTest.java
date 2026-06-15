package org.skyve.impl.job;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.skyve.job.JobScheduler;

@SuppressWarnings("static-method")
public class JobSchedulerStaticSingletonTest {

	private JobScheduler originalInstance;

	@Before
	public void setUp() {
		// Save the current instance so tests are isolated
		originalInstance = JobSchedulerStaticSingleton.get();
		// Reset to null for each test
		JobSchedulerStaticSingleton.set(null);
	}

	@After
	public void tearDown() {
		// Restore the original state
		JobSchedulerStaticSingleton.set(originalInstance);
	}

	@Test
	public void getReturnsNullInitially() {
		assertNull(JobSchedulerStaticSingleton.get());
	}

	@Test
	public void setAndGetReturnsSameInstance() {
		QuartzJobScheduler scheduler = new QuartzJobScheduler();
		JobSchedulerStaticSingleton.set(scheduler);
		assertSame(scheduler, JobSchedulerStaticSingleton.get());
	}

	@Test
	public void setDefaultInitialisesWithQuartzScheduler() {
		JobSchedulerStaticSingleton.setDefault();
		assertNotNull(JobSchedulerStaticSingleton.get());
	}
}
