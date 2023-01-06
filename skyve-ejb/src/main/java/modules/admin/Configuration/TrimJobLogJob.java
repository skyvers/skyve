package modules.admin.Configuration;

import java.time.LocalDateTime;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.types.DateTime;
import org.skyve.metadata.SortDirection;
import org.skyve.persistence.DocumentQuery;

import modules.admin.domain.Configuration;
import modules.admin.domain.Job;

public class TrimJobLogJob extends org.skyve.job.Job {

	@Override
	public String cancel() {
		return null;
	}

	/**
	 * The TrimJobLogJob finds all Jobs completed prior to the time of this job running minus the
	 * Configuration.jobLogRetentionDays and deletes them
	 * 
	 * If no value has been set and the job is configured to run, no job records will be deleted and
	 * the job will return early
	 */
	@Override
	public void execute() throws Exception {
		List<String> log = getLog();

		ConfigurationExtension configuration = Configuration.newInstance();
		Integer jobLogRetentionDays = configuration.getJobLogRetentionDays();

		if (jobLogRetentionDays == null) {
			log.add("Job Log Retention Days is not set in the Configuration. No Jobs will be trimmed.");
			setPercentComplete(100);
			return;
		}
		// calculate the most recent date to trim audits until from the job log retention days set in the configuration
		// screen
		DateTime jobLogRetentionEnd = new DateTime(LocalDateTime.now().minusDays(jobLogRetentionDays.longValue()));
		log.add("Starting Trim Job Log Job.");

		DocumentQuery q = CORE.getPersistence().newDocumentQuery(Job.MODULE_NAME,
				Job.DOCUMENT_NAME);
		q.getFilter().addNotNull(Job.endTimePropertyName);
		q.getFilter().addLessThan(Job.endTimePropertyName, jobLogRetentionEnd);
		q.addBoundOrdering(Job.endTimePropertyName, SortDirection.descending);
		List<Job> jobsToTrim = q.beanResults();
		log.add("Found " + jobsToTrim.size() + " logged job(s) to trim.");
		
		int jobNo = 1;
		for (Job job : jobsToTrim) {
			log.add("Trimming Job id: " + job.getBizId());
			CORE.getPersistence().delete(job);
			setPercentComplete(jobNo++, jobsToTrim.size());
		}
		log.add("Trim Log Job complete.");
		
		setPercentComplete(100);
	}

}
