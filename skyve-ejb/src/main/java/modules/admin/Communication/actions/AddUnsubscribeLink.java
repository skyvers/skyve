package modules.admin.Communication.actions;

import modules.admin.domain.Communication;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class AddUnsubscribeLink implements ServerSideAction<Communication> {
	
	private static final long serialVersionUID = 2886341074753936987L;

	private static final String UNSUBSCRIBE_LINK = "<a href=\"{unsubscribeUrl}\">Unsubscribe</a>";

	/**
	 * Add an unsubscribe link to the communication body.
	 */
	@Override
	public ServerSideActionResult<Communication> execute(Communication communication, WebContext webContext) throws Exception {

		Communication result = communication;

		String body = result.getBody();
		if (body == null) {
			result.setBody(UNSUBSCRIBE_LINK);
		}
		else {
			// check this has not already been added
			if (! body.toUpperCase().contains(UNSUBSCRIBE_LINK.toUpperCase())) {
				result.setBody(result.getBody() + "<p/>" + UNSUBSCRIBE_LINK);
			}
		}
		
		return new ServerSideActionResult<>(result);
	}
}
