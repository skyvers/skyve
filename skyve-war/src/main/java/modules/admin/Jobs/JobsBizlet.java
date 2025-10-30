package modules.admin.Jobs;

import org.skyve.metadata.model.document.Bizlet;

import jakarta.inject.Inject;
import modules.admin.domain.Jobs;

public class JobsBizlet extends Bizlet<Jobs> {
	public static final String SYSTEM_JOB_NOTIFICATION = "SYSTEM Job Notification";
	public static final String SYSTEM_JOB_NOTIFICATION_DEFAULT_SUBJECT = "Job - Complete";
	public static final String SYSTEM_JOB_NOTIFICATION_LINK_TO_JOBS = " Check <a href=\"{#context}?a=e&m=admin&d=Jobs\">Job log</a> for details.";
	public static final String SYSTEM_JOB_NOTICATION_DEFAULT_BODY = "The Job is complete." + SYSTEM_JOB_NOTIFICATION_LINK_TO_JOBS;

	@Inject
	private transient JobsService jobsService;

	@Override
	public Jobs newInstance(Jobs jobs) throws Exception {
		jobsService.refresh(jobs);
		return jobs;
	}
}
