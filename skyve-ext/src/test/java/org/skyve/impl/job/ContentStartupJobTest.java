package org.skyve.impl.job;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.util.UtilImpl;

@SuppressWarnings("static-method")
class ContentStartupJobTest {
	private OriginalConfiguration originalConfiguration;

	@BeforeEach
	void setUp() {
		originalConfiguration = new OriginalConfiguration(AbstractContentManager.IMPLEMENTATION_CLASS,
				UtilImpl.ENVIRONMENT_IDENTIFIER,
				UtilImpl.BOOTSTRAP_CUSTOMER);
		AbstractContentManager.IMPLEMENTATION_CLASS = RecordingContentManager.class;
		UtilImpl.ENVIRONMENT_IDENTIFIER = null;
		UtilImpl.BOOTSTRAP_CUSTOMER = null;
		RecordingContentManager.reset();
	}

	@AfterEach
	void tearDown() {
		AbstractContentManager.IMPLEMENTATION_CLASS = originalConfiguration.contentManagerClass();
		UtilImpl.ENVIRONMENT_IDENTIFIER = originalConfiguration.environmentIdentifier();
		UtilImpl.BOOTSTRAP_CUSTOMER = originalConfiguration.bootstrapCustomer();
	}

	@Test
	void executeStartsContentManager() {
		DataTierStartupJob job = new DataTierStartupJob();

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(RecordingContentManager.STARTED.get());
		assertTrue(RecordingContentManager.CLOSED.get());
	}

	@Test
	void executeSwallowsContentManagerStartupFailure() {
		RecordingContentManager.FAIL_STARTUP.set(true);
		DataTierStartupJob job = new DataTierStartupJob();

		assertDoesNotThrow(() -> job.execute(null));

		assertTrue(RecordingContentManager.STARTED.get());
		assertTrue(RecordingContentManager.CLOSED.get());
	}

	private record OriginalConfiguration(Class<? extends AbstractContentManager> contentManagerClass,
									 String environmentIdentifier,
									 String bootstrapCustomer) {
	}

	public static class RecordingContentManager extends NoOpContentManager {
		private static final AtomicBoolean STARTED = new AtomicBoolean();
		private static final AtomicBoolean CLOSED = new AtomicBoolean();
		private static final AtomicBoolean FAIL_STARTUP = new AtomicBoolean();

		static void reset() {
			STARTED.set(false);
			CLOSED.set(false);
			FAIL_STARTUP.set(false);
		}

		@Override
		public void startup() {
			STARTED.set(true);
			if (FAIL_STARTUP.get()) {
				throw new IllegalStateException("startup failed");
			}
		}

		@Override
		public void close() {
			CLOSED.set(true);
		}
	}
}
