package org.skyve.impl.job;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;

@SuppressWarnings("static-method")
class ContentStartupJobTest {
	private Class<? extends AbstractContentManager> originalContentManagerClass;

	@BeforeEach
	void setUp() {
		originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
		AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;
		RecordingContentManager.reset();
	}

	@AfterEach
	void tearDown() {
		AbstractContentManager.IMPLEMENTATION_CLASS = originalContentManagerClass;
	}

	@Test
	void executeStartsContentManager() {
		ContentStartupJob job = new ContentStartupJob();

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(RecordingContentManager.started);
		assertTrue(RecordingContentManager.closed);
	}

	@Test
	void executeSwallowsContentManagerStartupFailure() {
		RecordingContentManager.failStartup = true;
		ContentStartupJob job = new ContentStartupJob();

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(RecordingContentManager.started);
		assertTrue(RecordingContentManager.closed);
	}

	public static class RecordingContentManager extends NoOpContentManager {
		private static boolean started;
		private static boolean closed;
		private static boolean failStartup;

		static void reset() {
			started = false;
			closed = false;
			failStartup = false;
		}

		@Override
		public void startup() {
			started = true;
			if (failStartup) {
				throw new IllegalStateException("startup failed");
			}
		}

		@Override
		public void close() {
			closed = true;
		}
	}
}
