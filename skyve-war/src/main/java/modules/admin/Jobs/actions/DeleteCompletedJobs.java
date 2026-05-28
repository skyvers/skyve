package modules.admin.Jobs.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.domain.Jobs;

/**
 * Deletes completed job history entries from the admin job log document.
 */
public class DeleteCompletedJobs implements ServerSideAction<Jobs> {
	/**
	 * Removes persisted job log rows and returns the current jobs bean.
	 *
	 * @param bean
	 *        the jobs bean
	 * @param webContext
	 *        the current web context
	 * @return a result wrapping {@code bean}
	 * @throws Exception
	 *         if delete execution fails
	 */
	@Override
	public ServerSideActionResult<Jobs> execute(Jobs bean, WebContext webContext) 
	throws Exception {
		Persistence persistence = CORE.getPersistence();
		persistence.newBizQL("delete from {admin.Job} as job").execute();
		
		return new ServerSideActionResult<>(bean);
	}
}
