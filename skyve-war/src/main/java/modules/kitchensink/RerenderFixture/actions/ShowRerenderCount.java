package modules.kitchensink.RerenderFixture.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.kitchensink.RerenderFixture.RerenderFixtureBizlet;
import modules.kitchensink.domain.RerenderFixture;

/**
 * Shows how many rerenders of Week Ending Date the server was sent for the fixture.
 */
public class ShowRerenderCount implements ServerSideAction<RerenderFixture> {
	/**
	 * Copies the server's count of Week Ending Date's rerenders into Rerender Count.
	 *
	 * @param bean	The fixture.
	 * @param webContext	The current web context.
	 * @return	The fixture.
	 */
	@Override
	public ServerSideActionResult<RerenderFixture> execute(RerenderFixture bean, WebContext webContext) {
		bean.setRerenderCount(Integer.valueOf(RerenderFixtureBizlet.rerenders(bean.getBizId())));
		return new ServerSideActionResult<>(bean);
	}
}
