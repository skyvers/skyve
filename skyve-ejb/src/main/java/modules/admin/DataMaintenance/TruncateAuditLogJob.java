package modules.admin.DataMaintenance;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateTime;
import org.skyve.job.Job;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.CommunicationUtil;
import org.skyve.util.CommunicationUtil.ResponseMode;
import org.skyve.util.Util;

import modules.admin.DataMaintenance.actions.TruncateAuditLog;
import modules.admin.Jobs.JobsBizlet;
import modules.admin.domain.Audit;
import modules.admin.domain.DataMaintenance;

public class TruncateAuditLogJob extends Job {
	private static long MILLIS_IN_DAY = 86400000;

	@Override
	public String cancel() {
		return null;
	}

	@Override
	public void execute() throws Exception {
		List<String> log = getLog();
		String trace;

		DataMaintenance dm = (DataMaintenance) getBean();
		if (dm == null) {
			trace = "DataMaintenance bean cannot be retrieved. Exiting Truncate Audit Log Job.";
			Util.LOGGER.log(Level.WARNING, trace);
			log.add(trace);
			return;
		}
		
		// check a truncate audit log job or audit log refresh are not already running
		String auditResponse = dm.getAuditResponse();
		if (auditResponse != null) {
			trace = "Audit Log is currently being used. Please check the job log for "
					+ "a currently running Truncate Audit Log Job or Refresh Document Tuple Job.";
			Util.LOGGER.log(Level.INFO, trace);
			log.add(trace);
			setPercentComplete(100);
			return;
		}
		// if Audit log Retention Days is null all Audits are to be kept and this job will not delete anything
		Integer auditLogRetentionDays = dm.getAuditLogRetentionDays();
		if (auditLogRetentionDays == null) {
			trace = "Audit Log Retention is not set. No Audits are to be deleted.";
			Util.LOGGER.log(Level.INFO, trace);
			log.add(trace);
			setPercentComplete(100);
			return;
		}

		Persistence pers = CORE.getPersistence();
		log.add("Started Truncate Audit Log Job at " + new Date());
		
		long dayToPrune = auditLogRetentionDays.longValue();
		long customerEpochTime = dm.getEpochDate().toInstant().toEpochMilli();

		// Used for logging
		int dayInt = 1;
		int batchNo = 1;

		long pruneStartTime = System.currentTimeMillis() - (dayToPrune * MILLIS_IN_DAY);
		DocumentQuery q = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.getFilter().addLessThan(Audit.millisPropertyName, Long.valueOf(pruneStartTime));
		float size = q.beanResults().size();

		long pruneEndTime;

		List<String> auditsToTruncate = new ArrayList<>();

		// we want to keep checking and truncating Audits while the day to prune is greater than epoch and results are being
		// truncated
		// bucket Audits for truncation by day
		while (customerEpochTime < dayToPrune) {
			// percentage complete calculations
			pruneStartTime = System.currentTimeMillis() - (dayToPrune * MILLIS_IN_DAY);
			pruneEndTime = System.currentTimeMillis() - ((dayToPrune + 1) * MILLIS_IN_DAY);

			// percentage complete calculations
			q = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
			q.getFilter().addLessThan(Audit.millisPropertyName, Long.valueOf(pruneStartTime));
			float auditsLeftToProcess = q.beanResults().size();
			setPercentComplete((int) (1 - (auditsLeftToProcess / (size))));

			try (AutoClosingIterable<Audit> auditsToCheck = TruncateAuditLog.retrieveDaysAudits(pers, pruneStartTime, pruneEndTime)
					.beanIterable()) {
				log.add("Checking Day " + dayInt++ + " of Audits.");

				// check the day of audits for any audits we want to truncate
				// audits to truncate are batched in transactions of 100
				Iterator<Audit> iterator = auditsToCheck.iterator();
				for (Bean audit : auditsToCheck) {
					List<String> auditBizIds = TruncateAuditLog.retrieveAuditsToTruncate(pers, audit);
					for (String auditBizId : auditBizIds) {
						auditsToTruncate.add(auditBizId);
						if ((auditsToTruncate.size() % 100) == 0) {
							TruncateAuditLog.truncateAuditBatch(pers, auditsToTruncate);
							log.add("Truncated batch " + batchNo++ + ".");
							auditsToTruncate.clear();
						}
					}
				}
			}
			// update dayToPrune to the previous day
			dayToPrune = dayToPrune - MILLIS_IN_DAY;
		}

		if (!auditsToTruncate.isEmpty()) {
			TruncateAuditLog.truncateAuditBatch(pers, auditsToTruncate);
			log.add("Truncated batch " + batchNo++ + ".");
		}

		dm.setEpochDate(new DateTime(ZonedDateTime.now(ZoneOffset.UTC).withHour(0).withMinute(0).withSecond(0)
				.minusDays(auditLogRetentionDays.intValue()).toLocalDateTime()));
		dm = pers.save(dm);

		try {
			CommunicationUtil.sendFailSafeSystemCommunication(JobsBizlet.SYSTEM_JOB_NOTIFICATION,
					JobsBizlet.SYSTEM_JOB_NOTIFICATION_DEFAULT_SUBJECT, "Truncate Audit Log Job", ResponseMode.SILENT, null,
					dm);
		} catch (@SuppressWarnings("unused") Exception e) {
			log.add("Email notification failed.");
		}

		dm.setAuditResponse(null);
		setPercentComplete(100);
		log.add("Finished Truncation Job at " + new Date());
	}
}
