package modules.admin.Communication.actions;

import org.skyve.domain.app.admin.Communication.ActionType;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.CommunicationUtil;
import org.skyve.web.WebContext;

import modules.admin.domain.Communication;

/**
 * Triggers immediate sending of the configured communication batch.
 */
public class SendNow implements ServerSideAction<Communication> {
	/**
	 * Performs the execute operation.
	 * @param communication the communication value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public ServerSideActionResult<Communication> execute(Communication communication, WebContext webContext) throws Exception {
		communication.setActionType(ActionType.sendImmediately);
		
		Communication result = CommunicationUtil.kickOffJob(communication);
		
		return new ServerSideActionResult<>(result);
	}
}
