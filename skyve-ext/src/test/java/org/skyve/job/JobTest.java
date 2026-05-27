package org.skyve.job;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;

import org.junit.Test;
import org.skyve.domain.Bean;

@SuppressWarnings("static-method")
public class JobTest {
	private static class RecordingJob extends Job {
		@Override
		public void execute() {
			// no-op
		}
	}

	private static class RecordingCancellableJob extends CancellableJob {
		@Override
		public void execute() {
			// no-op
		}
	}

	@Test
	public void defaultCancelAndFlagsAreStable() {
		RecordingJob job = new RecordingJob();
		assertThat(job.cancel(), is("Job cannot be cancelled."));
		assertTrue(job.shouldRollbackOnCancel());
		assertFalse(job.shouldBeSilent());
		assertTrue(job.persistJobExecutionOnSuccess());
	}

	@Test
	public void executeOtherJobSetsContextBeforeInjectionFailure() {
		RecordingJob parent = new RecordingJob();
		RecordingJob child = new RecordingJob();
		Bean bean = mock(Bean.class);

		parent.setBean(bean);
		parent.getLog().add("seed");

		try {
			parent.execute(child);
			fail("Expected injection failure without CDI container");
		}
		catch (Exception e) {
			assertTrue(e instanceof IllegalStateException);
		}

		assertThat(child.getBean(), is(sameInstance(bean)));
		assertThat(child.getLog(), is(sameInstance(parent.getLog())));
		assertTrue(child.getLog().contains("seed"));
	}

	@Test
	public void cancellableJobTogglesCancelledState() {
		RecordingCancellableJob job = new RecordingCancellableJob();
		assertFalse(job.isCancelled());
		assertNull(job.cancel());
		assertTrue(job.isCancelled());
		assertTrue(job.shouldRollbackOnCancel());
		assertFalse(job.shouldBeSilent());
	}
}
