package modules.admin.UserList.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.UserList.BulkUserCreationJob;
import modules.admin.domain.UserList;

public class InviteUsers implements ServerSideAction<UserList> {

	
	private static final long serialVersionUID = -4884065778373508731L;

	@Override
	public ServerSideActionResult<UserList> execute(UserList bean, WebContext webContext)
			throws Exception {

		bean.setBulkCreateWithEmail(Boolean.TRUE);

		BulkUserCreationJob.kickoffJob(bean, webContext);

		return new ServerSideActionResult<>(bean);
	}
}
