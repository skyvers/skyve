package modules.kitchensink.ActionsFixture.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.ActionsFixture;

/**
 * Records that it ran; its only trigger is a button in the tab that Disable Tab disables.
 */
public class RunFromTabButton implements ServerSideAction<ActionsFixture> {
	/**
	 * Records in the outcome that the action ran.
	 *
	 * @param bean	The fixture.
	 * @param webContext	The current web context.
	 * @return	The fixture.
	 */
	@Override
	public ServerSideActionResult<ActionsFixture> execute(ActionsFixture bean, WebContext webContext) {
		bean.setOutcome("Run From Tab Button ran");
		return new ServerSideActionResult<>(bean);
	}
}
