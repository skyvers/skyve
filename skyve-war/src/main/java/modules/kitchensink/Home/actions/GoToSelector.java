package modules.kitchensink.Home.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.Home;

public class GoToSelector implements ServerSideAction<Home> {
	@Override
	public ServerSideActionResult<Home> execute(Home bean,
			WebContext webContext) {


		return new ServerSideActionResult<>(bean);
	}
}
