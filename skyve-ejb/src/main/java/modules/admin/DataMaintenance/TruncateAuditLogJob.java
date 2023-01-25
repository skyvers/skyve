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
import modules.admin.domain.DataMaintenance;

public class TruncateAuditLogJob extends Job {
	private static long MILLIS_IN_DAY = 86400000;

	@Override
	public String cancel() {
		return null;
	}

	/**
	 * Truncate Audit log Job
	 * 
	 * This job will
	 * 1. Check that it can retrieve the DataMaintenance bean for the logged in user, then retrieve and check the audit
	 * response - if it can't retrieve either or the audit response is not null, the job will exit
	 * 2. Check the Audit log Retention Days (ARD), if set the job will set how many days back to start checking and truncating
	 * Audits from, if not set the job will exit
	 * 3. While the day to prune is greater than the epoch date (e.g. ARD+1 days ago is later than the epoch date (last time Audits
	 * were truncated) 4 weeks ago)
	 * 3.1. Check there are more audits to process - if there are not, will exit the main loop
	 * 3.2. Retrieve all Audits with a unique auditBizId for the day
	 * 3.3. Process these Audits, adding bizIds of Audits to be deleted where appropriate to a List (auditsToTruncate)
	 * *when to delete audit types*
	 * Insert and Update Audit records - need to be checked that they aren't the penultimate record
	 * and that they are the Audit passed in or were earlier
	 * Reconstruction records - should all be deleted
	 * Delete records - all instances with the same auditBizId can be truncated
	 * 3.4. Once the auditsToTruncate reaches a size of 100, truncate all records in it through SQL and clear the List
	 * 4. Truncate all remaining Audits from the auditsToTruncate List
	 * 5. Update epochDate on the DataMaintenance bean
	 * 6. Email the user that ran the job
	 * 7. Set dataMaintenance.auditResponse to null so that this job or the Refresh Document Tuple Job can be run
	 * 
	 * @author Brandon Klar
	 */
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
		dm.setAuditResponse("Truncate Audit Log Job commenced.");
		dm = pers.save(dm);

		try {
			truncate(dm, pers);
		} finally {
			dm.setAuditResponse(null);
			dm = pers.save(dm);
		}

		setPercentComplete(100);
		log.add("Finished Truncation Job at " + new Date() + ".");
	}

	private void truncate(DataMaintenance bean, Persistence pers) throws Exception {
		DataMaintenance dm = bean;
		List<String> log = getLog();
		log.add("Started Truncate Audit Log Job at " + new Date());

		Integer auditLogRetentionDays = dm.getAuditLogRetentionDays();
		long dayToPrune = auditLogRetentionDays.longValue();
		// Customer epoch time is the last time that the audits have been pruned
		long customerEpochTime = dm.getEpochDate().toInstant().toEpochMilli();

		// Used for logging
		int dayInt = 1;
		int batchNo = 1;

		// Start time to prune
		long pruneStartTime = System.currentTimeMillis() - (dayToPrune * MILLIS_IN_DAY);

		// Find size for Percentage complete calculations
		SQL sql = pers.newSQL(
				"SELECT COUNT(*) AS count FROM ADM_Audit WHERE millis BETWEEN :epoch AND :pruneStartTime AND bizCustomer = :customer");
		sql.putParameter("pruneStartTime", Long.valueOf(pruneStartTime));
		sql.putParameter("epoch", Long.valueOf(dm.getEpochDate().toInstant().toEpochMilli()));
		sql.putParameter("customer", pers.getUser().getCustomerName(), false);
		Number size = sql.retrieveScalar(Number.class);
		log.add("Found " + size + " Audits to process.");
		long pruneEndTime;

		List<String> auditsToTruncate = new ArrayList<>();

		// We want to keep checking and truncating Audits while the day to prune is greater than epoch and results are being
		// truncated, starting from the Audit Retention log Days + 1 ago
		// Bucket Audits for truncation by day
		while (customerEpochTime < new DateTime(LocalDateTime.now(ZoneId.systemDefault()).minusDays(dayToPrune)
				.toEpochSecond(ZoneOffset.UTC)).toInstant().toEpochMilli()) {
			// percentage complete calculations
			pruneStartTime = System.currentTimeMillis() - (dayToPrune * MILLIS_IN_DAY);
			pruneEndTime = System.currentTimeMillis() - ((dayToPrune + 1) * MILLIS_IN_DAY);

			// Find number of audits left to process for Percentage complete calculations
			sql = pers.newSQL(
					"SELECT COUNT(*) FROM ADM_Audit WHERE millis BETWEEN :epoch AND :pruneStartTime AND bizCustomer = :customer");
			sql.putParameter("pruneStartTime", Long.valueOf(pruneStartTime));
			sql.putParameter("epoch", Long.valueOf(dm.getEpochDate().toInstant().toEpochMilli()));
			sql.putParameter("customer", pers.getUser().getCustomerName(), false);
			Number auditsLeftToProcess = sql.retrieveScalar(Number.class);
			log.add(auditsLeftToProcess + " Audits remaining to process.");

			setPercentComplete((int) (1 - (auditsLeftToProcess.floatValue() / (size.floatValue()))));

			// If there are no audits to process (no audits between current pruneStartTime and epoch), exit and stop retrieving
			// audits
			if (auditsLeftToProcess.equals(Integer.valueOf(0))) {
				break;
			}

			try (AutoClosingIterable<Bean> auditsToCheck = TruncateAuditLog.retrieveDaysAudits(pers, pruneStartTime,
					pruneEndTime)
					.projectedIterable()) {
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

	}
}
