package modules.admin.User.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.User.UserExtension;
import modules.admin.User.UserService;

public class Next implements ServerSideAction<UserExtension> {
	
	@Inject
	private transient UserService userService;
	
	@Override
	public ServerSideActionResult<UserExtension> execute(UserExtension adminUser, WebContext webContext)
	throws Exception {
		userService.next(adminUser);
		return new ServerSideActionResult<>(adminUser);
	}
	
}
