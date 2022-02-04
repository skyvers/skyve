package modules.admin.Startup.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.Startup.StartupExtension;

public class Dismiss implements ServerSideAction<StartupExtension> {

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
