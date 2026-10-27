package modules.kitchensink.ActionsFixture.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.ActionsFixture;

/**
 * Records that it ran; its only trigger is the change handler of Event Text, which Hide Event Text hides.
 */
public class RunFromEvent implements ServerSideAction<ActionsFixture> {
	/**
	 * Records in the outcome that the action ran.
	 *
	 * @param bean	The fixture.
	 * @param webContext	The current web context.
	 * @return	The fixture.
	 */
	@Override
	public ServerSideActionResult<ActionsFixture> execute(ActionsFixture bean, WebContext webContext) {
		bean.setOutcome("Run From Event ran");
		return new ServerSideActionResult<>(bean);
	}
}
