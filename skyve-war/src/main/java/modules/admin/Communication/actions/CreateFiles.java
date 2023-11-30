package modules.admin.Communication.actions;

import org.skyve.domain.app.admin.Communication.ActionType;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.CommunicationUtil;
import org.skyve.web.WebContext;

import modules.admin.domain.Communication;

public class CreateFiles implements ServerSideAction<Communication> {

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
