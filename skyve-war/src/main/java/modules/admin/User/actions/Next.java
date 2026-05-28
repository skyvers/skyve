package modules.admin.User.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.User.UserExtension;
import modules.admin.User.UserService;

/**
 * Advances the user wizard by delegating to {@link UserService}.
 */
public class Next implements ServerSideAction<UserExtension> {
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient UserService userService;

	/**
	 * Advances wizard state and validates the current step.
	 *
	 * @param adminUser The user wizard bean.
	 * @param webContext The current web context.
	 * @return The same user bean.
	 * @throws Exception If validation fails.
	 */
	@Override
	public ServerSideActionResult<UserExtension> execute(UserExtension adminUser, WebContext webContext)
			throws Exception {
		userService.next(adminUser);
		return new ServerSideActionResult<>(adminUser);
	}
}
