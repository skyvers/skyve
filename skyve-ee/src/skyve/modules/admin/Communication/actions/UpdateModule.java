package modules.admin.Communication.actions;

import modules.admin.domain.Communication;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class UpdateModule implements ServerSideAction<Communication> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Update the payment batch details.
	 */
	@Override
	public ServerSideActionResult<Communication> execute(Communication bean, WebContext webContext)
	throws Exception {
		
		bean.setDocumentName(null);
		
		return new ServerSideActionResult<>(bean);
	}
}
