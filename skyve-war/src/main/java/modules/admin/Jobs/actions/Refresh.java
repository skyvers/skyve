package modules.admin.Jobs.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.Jobs.JobsService;
import modules.admin.domain.Jobs;

/**
 * Refreshes the jobs edit view from current scheduler state.
 */
public class Refresh implements ServerSideAction<Jobs> {
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient JobsService jobsService;

	/**
	 * Rebuilds running job rows for the jobs dashboard.
	 *
	 * @param jobs
	 *        the jobs bean to refresh
	 * @param webContext
	 *        the current web context
	 * @return a result wrapping {@code jobs}
	 * @throws Exception
	 *         if refresh fails
	 */
	@Override
	public ServerSideActionResult<Jobs> execute(Jobs jobs, WebContext webContext)
			throws Exception {
		jobsService.refresh(jobs);
		return new ServerSideActionResult<>(jobs);
	}
}
