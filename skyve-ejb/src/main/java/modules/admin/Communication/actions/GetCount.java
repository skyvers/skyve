package modules.admin.Communication.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.CommunicationUtil;
import org.skyve.web.WebContext;

import modules.admin.domain.Communication;

public class GetCount implements ServerSideAction<Communication> {

	private static final long serialVersionUID = 60089863446674901L;

	@Override
	public ServerSideActionResult<Communication> execute(Communication communication, WebContext webContext) throws Exception {

		String results = CommunicationUtil.getResults(communication);
		
		communication.setResults(results);
		
		return new ServerSideActionResult<>(communication);
	}
}
