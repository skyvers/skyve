package org.skyve.impl.job;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.quartz.UnableToInterruptJobException;
import org.skyve.job.JobStatus;

public class AbstractSkyveJobTest {

	/**
	 * Minimal concrete subclass for testing AbstractSkyveJob's non-abstract behaviour.
	 */
	private static class TestJob extends AbstractSkyveJob {
		private boolean cancelled = false;

		@Override
		public void execute() throws Exception {
			// no-op
		}

		@Override
		public void execute(org.skyve.job.Job job) throws Exception {
			// no-op
		}

		@Override
		public String cancel() {
			if (cancelled) {
				return "already cancelled";
			}
			cancelled = true;
			return null; // null means cancellable
		}

		@Override
		public boolean shouldRollbackOnCancel() {
			return false;
		}

		@Override
		public boolean shouldBeSilent() {
			return false;
		}
	}

	/**
	 * A TestJob that cannot be cancelled.
	 */
	private static class UncancellableJob extends AbstractSkyveJob {
		@Override
		public void execute() throws Exception {
			// no-op
		}

		@Override
		public void execute(org.skyve.job.Job job) throws Exception {
			// no-op
		}

		@Override
		public String cancel() {
			return "cannot cancel";
		}

		@Override
		public boolean shouldRollbackOnCancel() {
			return true;
		}

		@Override
		public boolean shouldBeSilent() {
			return true;
		}
	}

	private TestJob job;

	@BeforeEach
	void setUp() {
		job = new TestJob();
	}

	// ── display name ────────────────────────────────────────────────────────

	@Test
	@SuppressWarnings("static-method")
	void displayNameIsNullByDefault() {
		assertNull(new TestJob().getDisplayName());
	}

	@Test
	void setDisplayNameStoresValue() {
		job.setDisplayName("My Job");
		assertEquals("My Job", job.getDisplayName());
	}

	// ── percent complete ─────────────────────────────────────────────────────

	@Test
	void percentCompleteStartsAtZero() {
		assertEquals(0, job.getPercentComplete());
	}

	@Test
	void setPercentCompleteStoresValue() {
		job.setPercentComplete(42);
		assertEquals(42, job.getPercentComplete());
	}

	@Test
	void setPercentCompleteFromTotalProcessedAndSize() {
		job.setPercentComplete(1, 4);
		assertEquals(25, job.getPercentComplete());
	}

	@Test
	void setPercentCompleteFromTotalsIsHundredWhenComplete() {
		job.setPercentComplete(10, 10);
		assertEquals(100, job.getPercentComplete());
	}

	// ── timestamps ──────────────────────────────────────────────────────────

	@Test
	void startTimeIsNotNullAfterConstruction() {
		assertNotNull(job.getStartTime());
	}

	@Test
	void endTimeIsNullBeforeExecution() {
		assertNull(job.getEndTime());
	}

	// ── status ──────────────────────────────────────────────────────────────

	@Test
	void statusIsNullByDefault() {
		assertNull(job.getStatus());
	}

	// ── log ─────────────────────────────────────────────────────────────────

	@Test
	void logIsEmptyByDefault() {
		assertNotNull(job.getLog());
		assertTrue(job.getLog().isEmpty());
	}

	@Test
	void logCanAcceptEntries() {
		job.getLog().add("first entry");
		assertEquals(1, job.getLog().size());
	}

	@Test
	void createLogDescriptionStringReturnsEmptyStringWhenLogIsEmpty() {
		assertEquals("", job.createLogDescriptionString());
	}

	@Test
	void createLogDescriptionStringReturnsAllEntries() {
		job.getLog().add("line one");
		job.getLog().add("line two");
		String desc = job.createLogDescriptionString();
		assertTrue(desc.contains("line one"));
		assertTrue(desc.contains("line two"));
	}

	// ── bean ────────────────────────────────────────────────────────────────

	@Test
	void beanIsNullByDefault() {
		assertNull(job.getBean());
	}

	@Test
	void setBeanStoresAndRetrievesNull() {
		job.setBean(null);
		assertNull(job.getBean());
	}

	// ── interrupt / cancel ──────────────────────────────────────────────────

	@Test
	void interruptSetsStatusToCancelledWhenJobIsCancellable() throws UnableToInterruptJobException {
		job.interrupt();
		assertEquals(JobStatus.cancelled, job.getStatus());
	}

	@Test
	@SuppressWarnings("static-method")
	void interruptThrowsUnableToInterruptWhenJobCannotBeCancelled() {
		UncancellableJob uncancellable = new UncancellableJob();
		assertThrows(UnableToInterruptJobException.class, uncancellable::interrupt);
	}

	// ── persistJobExecutionOnSuccess ─────────────────────────────────────────

	@Test
	void persistJobExecutionOnSuccessReturnsTrueByDefault() {
		assertTrue(job.persistJobExecutionOnSuccess());
	}

	// ── shouldRollbackOnCancel / shouldBeSilent ──────────────────────────────

	@Test
	void shouldRollbackOnCancelDelegatesToSubclass() {
		assertFalse(job.shouldRollbackOnCancel());
		assertTrue(new UncancellableJob().shouldRollbackOnCancel());
	}

	@Test
	void shouldBeSilentDelegatesToSubclass() {
		assertFalse(job.shouldBeSilent());
		assertTrue(new UncancellableJob().shouldBeSilent());
	}
}
