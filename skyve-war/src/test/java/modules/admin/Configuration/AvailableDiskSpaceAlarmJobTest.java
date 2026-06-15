package modules.admin.Configuration;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;
import org.skyve.domain.app.admin.Communication;

import modules.admin.domain.Configuration;
import modules.admin.domain.Generic;

@SuppressWarnings("static-method")
class AvailableDiskSpaceAlarmJobTest {

	@Test
	void cancelReturnsNull() {
		AvailableDiskSpaceAlarmJob job = new AvailableDiskSpaceAlarmJob();
		assertNull(job.cancel());
	}

	@Test
	void executeDoesNotSendWhenDiskSpaceIsAboveDefaultThreshold() throws Exception {
		TestableAvailableDiskSpaceAlarmJob job = new TestableAvailableDiskSpaceAlarmJob();
		job.snapshot = new Snapshot(2_000L, 50L, "<p>healthy</p>");

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(1, job.getLog().size());
		assertEquals("<p>healthy</p>", job.getLog().get(0));
		assertEquals(0, job.sentCount);
	}

	@Test
	void executeSendsWhenDiskSpaceIsBelowDefaultPercentageThreshold() throws Exception {
		TestableAvailableDiskSpaceAlarmJob job = new TestableAvailableDiskSpaceAlarmJob();
		job.snapshot = new Snapshot(2_000L, 9L, "<p>low percentage</p>");

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(1, job.sentCount);
		assertEquals("<p>low percentage</p>", job.sentGeneric.getMarkup1());
	}

	@Test
	void executeSendsWhenAvailableMegabytesAreBelowConfiguredThreshold() throws Exception {
		TestableAvailableDiskSpaceAlarmJob job = new TestableAvailableDiskSpaceAlarmJob();
		job.configuration.setAvailableDiskSpaceAlarmLevelPercentage(Integer.valueOf(5));
		job.configuration.setAvailableDiskSpaceAlarmLevelMB(Long.valueOf(1_500L));
		job.snapshot = new Snapshot(1_000L, 50L, "<p>low mb</p>");

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(1, job.sentCount);
		assertEquals(job.configuration, job.sentConfiguration);
		assertEquals(job.communication, job.sentCommunication);
		assertEquals("<p>low mb</p>", job.sentGeneric.getMarkup1());
		assertNotNull(job.sentGeneric.getText5001());
	}

	private static class TestableAvailableDiskSpaceAlarmJob extends AvailableDiskSpaceAlarmJob {
		private final ConfigurationExtension configuration = new ConfigurationExtension();
		private final Communication communication = mock(Communication.class);
		private Snapshot snapshot = new Snapshot(2_000L, 50L, "<p>healthy</p>");
		private int sentCount;
		private Communication sentCommunication;
		private Configuration sentConfiguration;
		private Generic sentGeneric;

		@Override
		protected ConfigurationExtension newConfiguration() {
			return configuration;
		}

		@Override
		protected DiskSpaceSnapshot newDiskSpaceSnapshot() {
			return snapshot;
		}

		@Override
		protected Communication initialiseSystemCommunication() {
			return communication;
		}

		@Override
		protected Generic newGeneric() {
			Generic generic = new Generic();
			generic.setBizId("generic-id");
			return generic;
		}

		@Override
		protected void send(Communication communicationToSend, Configuration configurationToSend, Generic genericToSend) {
			sentCount++;
			sentCommunication = communicationToSend;
			sentConfiguration = configurationToSend;
			sentGeneric = genericToSend;
		}
	}

	private record Snapshot(long totalAvailable, long totalAvailableLevel, String htmlSummary) implements AvailableDiskSpaceAlarmJob.DiskSpaceSnapshot {
		@Override
		public long getTotalAvailable() {
			return totalAvailable;
		}

		@Override
		public long getTotalAvailableLevel() {
			return totalAvailableLevel;
		}

		@Override
		public String getHTMLSummary() {
			return htmlSummary;
		}
	}
}
