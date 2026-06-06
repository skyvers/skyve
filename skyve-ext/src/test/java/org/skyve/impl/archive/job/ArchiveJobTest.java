package org.skyve.impl.archive.job;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.UtilImpl.ArchiveConfig;

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
}
