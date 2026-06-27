package org.skyve.impl.archive.job;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UtilImpl.ArchiveConfig;
import org.skyve.job.CancellableJob;

@SuppressWarnings("static-method")
class ArchiveJobTest {
	private final ArchiveConfig originalConfig = UtilImpl.ARCHIVE_CONFIG;

	@AfterEach
	void restoreConfig() {
		UtilImpl.ARCHIVE_CONFIG = originalConfig;
	}

	@Test
	void persistJobExecutionOnSuccessIsFalseWhenArchiveCronIsEnabled() {
		UtilImpl.ARCHIVE_CONFIG = new ArchiveConfig(10,
													100,
													List.of(),
													ArchiveConfig.DISABLED.cacheConfig(),
													new ArchiveConfig.ArchiveSchedule("* * * * *", "demo", "admin"));

		assertFalse(new ArchiveJob().persistJobExecutionOnSuccess());
	}

	@Test
	void persistJobExecutionOnSuccessIsTrueWhenArchiveCronIsDisabled() {
		UtilImpl.ARCHIVE_CONFIG = ArchiveConfig.DISABLED;

		assertTrue(new ArchiveJob().persistJobExecutionOnSuccess());
	}

	@Test
	void cancelWithoutRunningSubJobCancelsSelf() {
		ArchiveJob job = new ArchiveJob();

		assertNull(job.cancel());
		assertTrue(job.isCancelled());
	}

	@Test
	void cancelDelegatesToRunningSubJob() throws Exception {
		ArchiveJob job = new ArchiveJob();
		RecordingCancellableJob runningJob = new RecordingCancellableJob();
		Field field = ArchiveJob.class.getDeclaredField("runningJob");
		field.setAccessible(true);
		field.set(job, Optional.of(runningJob));

		assertNull(job.cancel());

		assertTrue(job.isCancelled());
		assertTrue(runningJob.cancelled);
	}

	private static final class RecordingCancellableJob extends CancellableJob {
		private boolean cancelled;

		@Override
		public void execute() {
			// not needed for cancel delegation
		}

		@Override
		public String cancel() {
			cancelled = true;
			return super.cancel();
		}
	}
}
