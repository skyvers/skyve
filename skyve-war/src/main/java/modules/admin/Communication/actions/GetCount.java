package modules.admin.Communication.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.CommunicationUtil;
import org.skyve.web.WebContext;

import modules.admin.domain.Communication;

/**
 * Returns the recipient count for the current communication selection.
 */
public class GetCount implements ServerSideAction<Communication> {
	/**
	 * Performs the execute operation.
	 * @param communication the communication value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public ServerSideActionResult<Communication> execute(Communication communication, WebContext webContext) throws Exception {

		String results = CommunicationUtil.getResults(communication);
		
		communication.setResults(results);
		
		return new ServerSideActionResult<>(communication);
	}
}
