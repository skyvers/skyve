package org.skyve.impl.job;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.job.JobScheduler;

@SuppressWarnings("static-method")
class JobSchedulerStaticSingletonTest {

	private JobScheduler originalInstance;

	@SuppressWarnings("null") // passes null on purpose
	@BeforeEach
	void setUp() {
		// Save the current instance so tests are isolated
		originalInstance = JobSchedulerStaticSingleton.get();
		// Reset to null for each test
		JobSchedulerStaticSingleton.set(null);
	}

	@AfterEach
	void tearDown() {
		// Restore the original state
		JobSchedulerStaticSingleton.set(originalInstance);
	}

	@Test
	void getReturnsNullInitially() {
		assertNull(JobSchedulerStaticSingleton.get());
	}

	@Test
	void setAndGetReturnsSameInstance() {
		QuartzJobScheduler scheduler = new QuartzJobScheduler();
		JobSchedulerStaticSingleton.set(scheduler);
		assertSame(scheduler, JobSchedulerStaticSingleton.get());
	}

	@Test
	void setDefaultInitialisesWithQuartzScheduler() {
		JobSchedulerStaticSingleton.setDefault();
		assertNotNull(JobSchedulerStaticSingleton.get());
	}
}
