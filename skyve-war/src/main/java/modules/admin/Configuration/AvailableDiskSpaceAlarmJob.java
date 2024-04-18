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

public class AvailableDiskSpaceAlarmJob extends Job {
	private static final Integer DEFAULT_AVAILABLE_DISK_SPACE_ALARM_LEVEL_PERCENTAGE = Integer.valueOf(10);
	private static final String AVAILABLE_DISK_SPACE_ALARM_NOFITICATION = "Available Disk Space Alarm Notification";
	private static final String AVAILABLE_DISK_SPACE_ALARM_DEFAULT_SEND_TO = "{startup.environmentSupportEmail}";
	private static final String AVAILABLE_DISK_SPACE_ALARM_DEFAULT_SUBJECT = "Disk space notification for {#context}";
	private static final String AVAILABLE_DISK_SPACE_ALARM_DEFAULT_BODY = "<p>" + UtilImpl.ARCHIVE_NAME
			+ " available disk space has fallen below the alarm level:</p><p>{markup1}</p>";

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
		ConfigurationExtension configuration = Configuration.newInstance();
		DiskSpaceSummary diskSpaceSummary = new DiskSpaceSummary();
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
			Communication communication = CommunicationUtil.initialiseSystemCommunication(AVAILABLE_DISK_SPACE_ALARM_NOFITICATION,
					AVAILABLE_DISK_SPACE_ALARM_DEFAULT_SEND_TO, null, AVAILABLE_DISK_SPACE_ALARM_DEFAULT_SUBJECT,
					AVAILABLE_DISK_SPACE_ALARM_DEFAULT_BODY);
			Generic generic = Generic.newInstance();
			generic.setMarkup1(htmlSummary);
			CommunicationUtil.send(communication, RunMode.ACTION, ResponseMode.SILENT, null, configuration, generic);
		}

		setPercentComplete(100);
	}
}
