package modules.admin.Job.actions;

import org.skyve.EXT;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.Job;

public class CancelJob implements ServerSideAction<Job> {
	@Override
	public ServerSideActionResult<Job> execute(Job bean, WebContext webContext) throws Exception {
		String instanceId = bean.getInstanceId();

		boolean result = EXT.getJobScheduler().cancelJob(instanceId);
		if (result) {
			// Cancelled job
			webContext.growl(MessageSeverity.info, "Job Cancelled");
		}
		else {
			// Couldn't cancel, no exception, probably already cancelled
			webContext.growl(MessageSeverity.warn, "Unable to cancel job");
		}

		return new ServerSideActionResult<>(bean);
	}
}
