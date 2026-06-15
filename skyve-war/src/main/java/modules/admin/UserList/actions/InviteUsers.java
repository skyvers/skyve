package modules.admin.UserList.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.UserList.BulkUserCreationJob;
import modules.admin.domain.UserList;

/**
 * Starts bulk user creation with invitation email delivery enabled.
 */
public class InviteUsers implements ServerSideAction<UserList> {
	/**
	 * Sets invitation mode and submits the bulk creation job.
	 *
	 * @param bean The bulk invitation request bean.
	 * @param webContext The current web context.
	 * @return The same request bean.
	 * @throws Exception If validation or job submission fails.
	 */
	@Override
	public ServerSideActionResult<UserList> execute(UserList bean, WebContext webContext)
			throws Exception {
		bean.setBulkCreateWithEmail(Boolean.TRUE);

		BulkUserCreationJob.kickoffJob(bean, webContext);

		return new ServerSideActionResult<>(bean);
	}
}
