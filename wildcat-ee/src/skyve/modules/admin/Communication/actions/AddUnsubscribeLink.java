package modules.admin.Communication.actions;

import modules.admin.domain.Communication;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class AddUnsubscribeLink implements ServerSideAction<Communication> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	private static final String UNSUBSCRIBE_LINK = "<a href=\"{unsubscribeUrl}\">Unsubscribe</a>";

	/**
	 * Kick off the annual returns job.
	 */
	@Override
	public ServerSideActionResult execute(Communication communication, WebContext webContext) throws Exception {

		Communication result = communication;

		// check this has not already been added
		if (!result.getBody().toUpperCase().contains(UNSUBSCRIBE_LINK.toUpperCase())) {
			result.setBody(result.getBody() + "<p/>" + UNSUBSCRIBE_LINK);
		}
		return new ServerSideActionResult(result);
	}
}
