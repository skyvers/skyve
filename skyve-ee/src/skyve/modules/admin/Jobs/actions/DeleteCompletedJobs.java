package modules.admin.Jobs.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.domain.Jobs;

public class DeleteCompletedJobs implements ServerSideAction<Jobs> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -1037253249182913062L;

	@Override
	public ServerSideActionResult<Jobs> execute(Jobs bean, WebContext webContext) 
	throws Exception {
		Persistence persistence = CORE.getPersistence();
		persistence.newBizQL("delete from {admin.Job} as job").execute();
		
		return new ServerSideActionResult<>(bean);
	}
}
