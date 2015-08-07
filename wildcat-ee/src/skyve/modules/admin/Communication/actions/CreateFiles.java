package modules.admin.Communication.actions;

import modules.admin.Communication.CommunicationBizlet;
import modules.admin.domain.Communication;
import modules.admin.domain.Communication.ActionType;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class CreateFiles implements ServerSideAction<Communication> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Kick off the annual returns job.
	 */
	@Override
	public ServerSideActionResult execute(Communication communication, WebContext webContext) throws Exception {

		communication.setActionType(ActionType.saveForBulkSend);
		
		if(communication.getFilePath()==null){
			throw new ValidationException(new Message(Communication.filePathPropertyName, "Value is required for this action"));
		}
		
		Communication result = CommunicationBizlet.kickOffJob(communication);
		
		return new ServerSideActionResult(result);
	}
}
