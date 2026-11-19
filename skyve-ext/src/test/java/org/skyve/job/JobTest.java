package org.skyve.job;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;

@SuppressWarnings("static-method")
class JobTest {
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
	void defaultCancelAndFlagsAreStable() {
		RecordingJob job = new RecordingJob();
		assertThat(job.cancel(), is("Job cannot be cancelled."));
		assertTrue(job.shouldRollbackOnCancel());
		assertFalse(job.shouldBeSilent());
		assertTrue(job.persistJobExecutionOnSuccess());
	}

	@Test
	void executeOtherJobSetsContextBeforeInjectionFailure() {
		RecordingJob parent = new RecordingJob();
		RecordingJob child = new RecordingJob();
		Bean bean = mock(Bean.class);

		parent.setBean(bean);
		parent.getLog().add("seed");

		// injection fails without a CDI container
		assertThrows(IllegalStateException.class, () -> parent.execute(child));

		assertThat(child.getBean(), is(sameInstance(bean)));
		assertThat(child.getLog(), is(sameInstance(parent.getLog())));
		assertTrue(child.getLog().contains("seed"));
	}

	@Test
	void cancellableJobTogglesCancelledState() {
		RecordingCancellableJob job = new RecordingCancellableJob();
		assertFalse(job.isCancelled());
		assertNull(job.cancel());
		assertTrue(job.isCancelled());
		assertTrue(job.shouldRollbackOnCancel());
		assertFalse(job.shouldBeSilent());
	}
}
