package modules.admin.UserList.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.UserList.BulkUserCreationJob;
import modules.admin.domain.UserList;

public class CreateUsers implements ServerSideAction<UserList> {

	
	@Override
	public ServerSideActionResult<UserList> execute(UserList bean, WebContext webContext)
			throws Exception {

		bean.setBulkCreateWithEmail(Boolean.FALSE);

		BulkUserCreationJob.kickoffJob(bean, webContext);

		return new ServerSideActionResult<>(bean);
	}


}
