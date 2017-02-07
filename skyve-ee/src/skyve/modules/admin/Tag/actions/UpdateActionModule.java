package modules.admin.Tag.actions;

import modules.admin.domain.Tag;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class UpdateActionModule implements ServerSideAction<Tag> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Update the payment batch details.
	 */
	@Override
	public ServerSideActionResult<Tag> execute(Tag bean, WebContext webContext)
	throws Exception {
		
		bean.setActionDocumentName(null);
		UpdateActionDocument.resetDocument(bean);
		
		return new ServerSideActionResult<>(bean);
	}
}
