package modules.kitchensink.ActionsFixtureRow.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.ActionsFixtureRow;

/**
 * Records that it ran for a row; its only trigger is the row's link in the actions fixture's rows grid.
 * PrimeFaces applies this action's conditions to that link, so Disable Row Action disables it there;
 * SmartClient and the server check apply only the link's own condition, so Hide Link hides it.
 */
public class RunFromRow implements ServerSideAction<ActionsFixtureRow> {
	/**
	 * Records in the row's outcome that the action ran.
	 *
	 * @param bean	The row.
	 * @param webContext	The current web context.
	 * @return	The row.
	 */
	@Override
	public ServerSideActionResult<ActionsFixtureRow> execute(ActionsFixtureRow bean, WebContext webContext) {
		bean.setOutcome("Run From Row ran");
		return new ServerSideActionResult<>(bean);
	}
}
