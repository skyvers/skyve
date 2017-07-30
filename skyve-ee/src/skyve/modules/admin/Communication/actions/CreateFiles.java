package modules.admin.Communication.actions;

import modules.admin.Communication.CommunicationUtil;
import modules.admin.domain.Communication;
import modules.admin.domain.Communication.ActionType;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class CreateFiles implements ServerSideAction<Communication> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Kick off the job to generate email files for bulk send.
	 */
	@Override
	public ServerSideActionResult<Communication> execute(Communication communication, WebContext webContext) throws Exception {

		communication.setActionType(ActionType.saveForBulkSend);
		
		Communication result = CommunicationUtil.kickOffJob(communication);
		result.setRefreshBatches(Boolean.TRUE);
		
		return new ServerSideActionResult<>(result);
	}
}
