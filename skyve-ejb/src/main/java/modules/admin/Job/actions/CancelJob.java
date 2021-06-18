package modules.admin.Job.actions;

import org.quartz.UnableToInterruptJobException;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;
import modules.admin.domain.Job;

public class CancelJob implements ServerSideAction<Job> {
	private static final long serialVersionUID = 2335124953478958973L;

	@Override
	public ServerSideActionResult<Job> execute(Job bean, WebContext webContext) throws Exception {
		String instanceId = bean.getInstanceId();

		try {
			boolean result = JobScheduler.cancelJob(instanceId);
			if (result) {
				// Cancelled job
				webContext.growl(MessageSeverity.info, "Job Cancelled");
			}
			else {
				// Couldn't cancel, no exception, probably already cancelled
				webContext.growl(MessageSeverity.warn, "Unable to cancel job");
			}
		}
		catch (UnableToInterruptJobException e) {
			String reason = e.getMessage();
			// If the job returns anything other than null from its
			// cancel() an UnableToInterruptJobException will be thrown with the reason contained
			webContext.growl(MessageSeverity.warn, "Unable to cancel job: " + reason);
		}

		return new ServerSideActionResult<>(bean);
	}
}
