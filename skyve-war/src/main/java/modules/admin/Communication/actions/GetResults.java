package modules.admin.Communication.actions;

import org.skyve.domain.app.admin.Communication.ActionType;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.CommunicationUtil;
import org.skyve.web.WebContext;

import modules.admin.domain.Communication;

/**
 * Returns file-generation results for the current communication run.
 */
public class GetResults implements ServerSideAction<Communication> {
	/**
	 * Performs the execute operation.
	 * @param communication the communication value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public ServerSideActionResult<Communication> execute(Communication communication, WebContext webContext) throws Exception {
		communication.setActionType(ActionType.testBindingsAndOutput);

		String results = CommunicationUtil.getResults(communication);

		communication.setResults(results);

		Communication result = CommunicationUtil.kickOffJob(communication);

		return new ServerSideActionResult<>(result);
	}
}
