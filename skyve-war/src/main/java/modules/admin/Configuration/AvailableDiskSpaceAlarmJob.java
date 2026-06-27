package modules.admin.Configuration;

import java.util.List;

import org.skyve.domain.app.admin.Communication;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.Job;
import org.skyve.util.CommunicationUtil;
import org.skyve.util.CommunicationUtil.ResponseMode;
import org.skyve.util.CommunicationUtil.RunMode;

import modules.admin.domain.Configuration;
import modules.admin.domain.Generic;

/**
 * Raises an alarm when available disk space drops below the configured threshold.
 */
public class AvailableDiskSpaceAlarmJob extends Job {
	private static final Integer DEFAULT_AVAILABLE_DISK_SPACE_ALARM_LEVEL_PERCENTAGE = Integer.valueOf(10);
	private static final String AVAILABLE_DISK_SPACE_ALARM_NOFITICATION = "Available Disk Space Alarm Notification";
	private static final String AVAILABLE_DISK_SPACE_ALARM_DEFAULT_SEND_TO = "{startup.environmentSupportEmail}";
	private static final String AVAILABLE_DISK_SPACE_ALARM_DEFAULT_SUBJECT = "Disk space notification for {#context}";
	private static final String AVAILABLE_DISK_SPACE_ALARM_DEFAULT_BODY = "<p>{text5001} available disk space has fallen below the alarm level:</p><p>{markup1}</p>";

	/**
	 * Performs the cancel operation.
	 * @return the operation result
	 */
	@Override
	public String cancel() {
		return null;
	}

	/**
	 * The AvailableDiskSpaceAlarmJob checks the available disk space and sends a
	 * Communication notification if the available disk space is below the value set
	 * in Configuration.availableDiskSpaceAlarmLevel
	 * 
	 * If no value has been set and the job is configured to run, the default alarm
	 * level will be 10%
	 */
	@Override
	public void execute() throws Exception {
		List<String> log = getLog();

		// evaluate whether alarm should be sent
		ConfigurationExtension configuration = newConfiguration();
		DiskSpaceSnapshot diskSpaceSummary = newDiskSpaceSnapshot();
		String htmlSummary = diskSpaceSummary.getHTMLSummary();
		log.add(htmlSummary);

		Integer percentageLevel = configuration.getAvailableDiskSpaceAlarmLevelPercentage();
		Long levelMB = configuration.getAvailableDiskSpaceAlarmLevelMB();

		// DEFAULT if level has not been set
		if (percentageLevel == null) {
			percentageLevel = DEFAULT_AVAILABLE_DISK_SPACE_ALARM_LEVEL_PERCENTAGE;
		}
		if ((diskSpaceSummary.getTotalAvailableLevel() <= percentageLevel.longValue()) ||
				(levelMB != null && (diskSpaceSummary.getTotalAvailable() <= levelMB.longValue()))) {
			Communication communication = initialiseSystemCommunication();
			Generic generic = newGeneric();
			generic.setMarkup1(htmlSummary);
			// nameEnv is the application name and environment identifier.
			StringBuilder nameEnv = new StringBuilder();
			nameEnv.append("[").append(UtilImpl.ARCHIVE_NAME);
			if (UtilImpl.ENVIRONMENT_IDENTIFIER != null) {
				nameEnv.append(" - ").append(UtilImpl.ENVIRONMENT_IDENTIFIER);
			}
			nameEnv.append("]");
			generic.setText5001(nameEnv.toString());
			send(communication, configuration, generic);
		}

		setPercentComplete(100);
	}

	@SuppressWarnings("static-method") // test seam
	protected ConfigurationExtension newConfiguration() {
		return Configuration.newInstance();
	}

	@SuppressWarnings("static-method") // test seam
	protected DiskSpaceSnapshot newDiskSpaceSnapshot() {
		DiskSpaceSummary summary = new DiskSpaceSummary();
		return new DiskSpaceSummarySnapshot(summary);
	}

	@SuppressWarnings("static-method") // test seam
	protected Communication initialiseSystemCommunication() throws Exception {
		return CommunicationUtil.initialiseSystemCommunication(AVAILABLE_DISK_SPACE_ALARM_NOFITICATION,
				AVAILABLE_DISK_SPACE_ALARM_DEFAULT_SEND_TO, null, AVAILABLE_DISK_SPACE_ALARM_DEFAULT_SUBJECT,
				AVAILABLE_DISK_SPACE_ALARM_DEFAULT_BODY);
	}

	@SuppressWarnings("static-method") // test seam
	protected Generic newGeneric() {
		return Generic.newInstance();
	}

	@SuppressWarnings("static-method") // test seam
	protected void send(Communication communication, Configuration configuration, Generic generic) throws Exception {
		CommunicationUtil.send(communication, RunMode.ACTION, ResponseMode.SILENT, null, configuration, generic);
	}

	protected interface DiskSpaceSnapshot {
		long getTotalAvailable();

		long getTotalAvailableLevel();

		String getHTMLSummary();
	}

	private static class DiskSpaceSummarySnapshot implements DiskSpaceSnapshot {
		private final DiskSpaceSummary summary;

		private DiskSpaceSummarySnapshot(DiskSpaceSummary summary) {
			this.summary = summary;
		}

		@Override
		public long getTotalAvailable() {
			return summary.getTotalAvailable();
		}

		@Override
		public long getTotalAvailableLevel() {
			return summary.getTotalAvailableLevel();
		}

		@Override
		public String getHTMLSummary() {
			return summary.getHTMLSummary();
		}
	}
}
