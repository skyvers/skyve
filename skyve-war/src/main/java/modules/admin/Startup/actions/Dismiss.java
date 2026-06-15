package modules.admin.Startup.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.Startup.StartupExtension;

/**
 * Dismisses the startup prompt and optionally persists the
 * "don't show again" preference.
 */
public class Dismiss implements ServerSideAction<StartupExtension> {
	/**
	 * Handles startup dismissal requests.
	 *
	 * @param bean the startup bean carrying dismissal choice
	 * @param webContext the current web context
	 * @return the action result wrapping the startup bean
	 * @throws Exception if configuration update fails
	 */
	@Override
	public ServerSideActionResult<StartupExtension> execute(StartupExtension bean, WebContext webContext) throws Exception {

		// check if the user ticket don't show again
		if (Boolean.TRUE.equals(bean.getDontShowAgain())) {
			// update the configuration
			bean.setDontShow();
		}

		return new ServerSideActionResult<>(bean);
	}
}
