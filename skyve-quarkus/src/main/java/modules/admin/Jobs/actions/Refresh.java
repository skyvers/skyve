package modules.admin.Jobs.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.Jobs.JobsBizlet;
import modules.admin.domain.Jobs;

/**
 * Refresh the edit view
 */
public class Refresh implements ServerSideAction<Jobs> {
	@Override
	public ServerSideActionResult<Jobs> execute(Jobs jobs, WebContext webContext)
	throws Exception {
		JobsBizlet.refresh(jobs);
		return new ServerSideActionResult<>(jobs);
	}
}
