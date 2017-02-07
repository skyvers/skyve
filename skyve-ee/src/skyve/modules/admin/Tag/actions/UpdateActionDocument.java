package modules.admin.Tag.actions;

import modules.admin.domain.Tag;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class UpdateActionDocument implements ServerSideAction<Tag> {
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
		
		resetDocument(bean);
		
		return new ServerSideActionResult<>(bean);
	}
	
	public static void resetDocument(Tag bean){
		
		bean.setDocumentAction(null);
	}
}
