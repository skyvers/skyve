package modules.admin.Communication.actions;

import modules.admin.Communication.CommunicationUtil;
import modules.admin.domain.Communication;
import modules.admin.domain.Communication.ActionType;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class SendNow implements ServerSideAction<Communication> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Kick off the annual returns job.
	 */
	@Override
	public ServerSideActionResult<Communication> execute(Communication communication, WebContext webContext) throws Exception {

		communication.setActionType(ActionType.sendImmediately);
		
		Communication result = CommunicationUtil.kickOffJob(communication);
		
		return new ServerSideActionResult<>(result);
	}
}
