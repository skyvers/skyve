package modules.admin.Communication.actions;

import modules.admin.Communication.CommunicationBizlet;
import modules.admin.Tag.TagBizlet;
import modules.admin.domain.Communication;
import modules.admin.domain.Communication.ActionType;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class GetResults implements ServerSideAction<Communication> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Kick off the annual returns job.
	 */
	@Override
	public ServerSideActionResult execute(Communication communication, WebContext webContext) throws Exception {

		communication.setActionType(ActionType.testBindingsAndOutput);
	
		String results = getResults(communication);
		
		communication.setResults(results);
		
		Communication result = CommunicationBizlet.kickOffJob(communication);
		
		return new ServerSideActionResult(result);
	}
	
	public static String getResults(Communication communication) throws Exception{
		
		if (communication.getTag() == null) {
			throw new ValidationException(new Message(Communication.tagPropertyName, "A tag must be selected for results."));
		}

		CommunicationBizlet.checkForUnsavedData(communication);

		Long count = TagBizlet.getTaggedCountForDocument(communication.getTag(), communication.getModuleName(), communication.getDocumentName());

		StringBuilder results = new StringBuilder();
		results.append(count).append(" communications for ");
		results.append(communication.getDocumentName());
		results.append(" will be ");
		
		switch (communication.getActionType()){
		case saveForBulkSend:
			results.append(" created into the directory ");
			results.append(communication.getFilePath());
			results.append(".");
			break;
			
		case sendImmediately:
			results.append(" sent immediately.");
			break;
			
		case testBindingsAndOutput:
			results.append(" tested.");
			break;
			
		default:
			break;
		}
		
		return results.toString();
		
	}
}
