package modules.admin.Jobs;

import java.util.List;

import org.skyve.EXT;
import org.skyve.job.JobDescription;
import org.skyve.metadata.model.document.Bizlet;

import modules.admin.Job.JobExtension;
import modules.admin.domain.Job;
import modules.admin.domain.Jobs;

public class JobsBizlet extends Bizlet<Jobs> {
	public static final String SYSTEM_JOB_NOTIFICATION = "SYSTEM Job Notification";
	public static final String SYSTEM_JOB_NOTIFICATION_DEFAULT_SUBJECT = "Job - Complete";
	public static final String SYSTEM_JOB_NOTIFICATION_LINK_TO_JOBS = " Check <a href=\"{#context}?a=e&m=admin&d=Jobs\">Job log</a> for details.";
	public static final String SYSTEM_JOB_NOTICATION_DEFAULT_BODY = "The Job is complete." + SYSTEM_JOB_NOTIFICATION_LINK_TO_JOBS;

	@Override
	public Jobs newInstance(Jobs jobs) throws Exception {
		refresh(jobs);
		return jobs;
	}
	
	public static final void refresh(Jobs jobs) throws Exception {
		List<JobExtension> runningJobs = jobs.getRunningJobs();
		runningJobs.clear();
		
		for (JobDescription jd : EXT.getJobScheduler().getCustomerRunningJobs()) {
			// the job could be finished but the thread is still sleeping waiting for the last UI poll
			if (jd.getStatus() == null) { // not finished
				JobExtension job = Job.newInstance();
				job.setStartTime(jd.getStartTime());
				job.setDisplayName(jd.getName());
				job.setPercentComplete(Integer.valueOf(jd.getPercentComplete()));
				job.setLog(jd.getLogging());
				job.setInstanceId(jd.getInstanceId());
				runningJobs.add(job);
			}
		}
	}
}
