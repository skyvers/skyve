package modules.admin.Communication.actions;

import modules.admin.domain.Communication;

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
	public ServerSideActionResult execute(Communication mailout, WebContext webContext)
	throws Exception {

		
		if (mailout.getTag() == null) {
			throw new ValidationException(new Message(Communication.tagPropertyName, 
														"A tag must be selected for results."));
		}
		
//		CommunicationBizlet.checkForUnsavedData(mailout);		
		
		StringBuilder results =	new StringBuilder();
		results.append("\nEmails will be created into the directory ");
		
		
		mailout.setResults(results.toString());
		
		mailout.originalValues().remove(Communication.resultsPropertyName);
		
		return new ServerSideActionResult(mailout);
	}	
}
