package modules.admin.Jobs.actions;

import modules.admin.Jobs.JobsBizlet;
import modules.admin.domain.Jobs;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

/**
 * Refresh the edit view
 */
public class Refresh implements ServerSideAction<Jobs> {
	private static final long serialVersionUID = -2274023750260116612L;

	@Override
	public ServerSideActionResult<Jobs> execute(Jobs jobs, WebContext webContext)
	throws Exception {
		JobsBizlet.refresh(jobs);
		return new ServerSideActionResult<>(jobs);
	}
}
