package modules.admin.DataMaintenance;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateTime;
import org.skyve.job.Job;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
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
		// Customer epoch time is the last time that the audits have been pruned
		long customerEpochTime = dm.getEpochDate().toInstant().toEpochMilli();

		// Used for logging
		int dayInt = 1;
		int batchNo = 1;

		// Start time to prune
		long pruneStartTime = System.currentTimeMillis() - (dayToPrune * MILLIS_IN_DAY);

		// Find size for Percentage complete calculations
		SQL sql = pers.newSQL("SELECT COUNT(*) FROM ADM_Audit WHERE millis < :pruneStartTime AND bizCustomer = :customer");
		sql.putParameter("pruneStartTime", Long.valueOf(pruneStartTime));
		sql.putParameter("customer", pers.getUser().getCustomerName(), false);
		Long size = (Long) sql.retrieveTuple()[0];

		long pruneEndTime;

		List<String> auditsToTruncate = new ArrayList<>();

		// We want to keep checking and truncating Audits while the day to prune is greater than epoch and results are being
		// truncated, starting from the Audit Retention log Days + 1 ago
		// Bucket Audits for truncation by day
		while (customerEpochTime < (LocalDateTime.now(ZoneId.systemDefault()).minusDays(dayToPrune))
				.toEpochSecond(ZoneOffset.UTC)) {
			// percentage complete calculations
			pruneStartTime = System.currentTimeMillis() - (dayToPrune * MILLIS_IN_DAY);
			pruneEndTime = System.currentTimeMillis() - ((dayToPrune + 1) * MILLIS_IN_DAY);

			// Find number of audits left to process for Percentage complete calculations
			sql = pers.newSQL("SELECT COUNT(*) FROM ADM_Audit WHERE millis < :pruneStartTime AND bizCustomer = :customer");
			sql.putParameter("pruneStartTime", Long.valueOf(pruneStartTime));
			sql.putParameter("customer", pers.getUser().getCustomerName(), false);

			Long auditsLeftToProcess = (Long) sql.retrieveTuple()[0];
			setPercentComplete((int) (1 - (auditsLeftToProcess.floatValue() / (size.floatValue()))));

			try (AutoClosingIterable<Audit> auditsToCheck = TruncateAuditLog.retrieveDaysAudits(pers, pruneStartTime, pruneEndTime)
					.beanIterable()) {
				log.add("Checking Day " + dayInt++ + " of Audits.");

				// Check the day of audits for any audits we want to truncate
				for (Bean audit : auditsToCheck) {
					// Retrieve all Audits for the same bizId
					List<String> auditBizIds = TruncateAuditLog.retrieveAuditsToTruncate(pers, audit);
					for (String auditBizId : auditBizIds) {
						auditsToTruncate.add(auditBizId);
						// Audits to truncate are batched in transactions of 100
						if ((auditsToTruncate.size() % 100) == 0) {
							TruncateAuditLog.truncateAuditBatch(pers, auditsToTruncate);
							log.add("Truncated batch " + batchNo++ + ".");
							auditsToTruncate.clear();
						}
					}
				}
			}
			// Update dayToPrune to the previous day
			dayToPrune = dayToPrune + 1;
		}

		if (!auditsToTruncate.isEmpty()) {
			TruncateAuditLog.truncateAuditBatch(pers, auditsToTruncate);
			log.add("Truncated batch " + batchNo++ + ".");
		}

		// Update epoch time of the system to the current datetime minus the Audit log Retention Days
		dm.setEpochDate(new DateTime(
				ZonedDateTime.now(ZoneId.systemDefault()).minusDays(auditLogRetentionDays.intValue()).toLocalDateTime()));
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
