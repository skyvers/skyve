package modules.admin.Jobs;

import java.util.List;

import modules.admin.domain.Job;
import modules.admin.domain.Jobs;

import org.skyve.job.JobDescription;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.model.document.Bizlet;

public class JobsBizlet extends Bizlet<Jobs> {
	
	public static final String SYSTEM_JOB_NOTIFICATION = "SYSTEM Job Notification";
	public static final String SYSTEM_JOB_NOTIFICATION_DEFAULT_SUBJECT = "Job - Complete";
	public static final String SYSTEM_JOB_NOTICATION_DEFAULT_BODY = "The Job is complete.";

	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2374495221430654562L;

	
	
	@Override
	public Jobs newInstance(Jobs jobs) throws Exception {
		refresh(jobs);
		return jobs;
	}
	
	public static final void refresh(Jobs jobs) throws Exception {
		
		List<Job> runningJobs = jobs.getRunningJobs();
		runningJobs.clear();
		
		for (JobDescription jd : JobScheduler.getCustomerRunningJobs()) {
			// the job could be finished but the thread is still sleeping waiting for the last UI poll
			if (jd.getStatus() == null) { // not finished
				Job job = Job.newInstance();
				job.setStartTime(jd.getStartTime());
				job.setDisplayName(jd.getName());
				job.setPercentComplete(new Integer(jd.getPercentComplete()));
				job.setLog(jd.getLogging());
				
				runningJobs.add(job);
			}
		}
	}
}
