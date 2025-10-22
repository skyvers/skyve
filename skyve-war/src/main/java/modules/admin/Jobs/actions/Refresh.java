package modules.admin.Jobs.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.Jobs.JobsService;
import modules.admin.domain.Jobs;

/**
 * Refresh the edit view
 */
public class Refresh implements ServerSideAction<Jobs> {
	@Inject
	private transient JobsService jobsService;
	
	@Override
	public ServerSideActionResult<Jobs> execute(Jobs jobs, WebContext webContext)
	throws Exception {
		jobsService.refresh(jobs);
		return new ServerSideActionResult<>(jobs);
	}
}
