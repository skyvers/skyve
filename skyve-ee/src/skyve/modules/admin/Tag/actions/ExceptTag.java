package modules.admin.Tag.actions;

import modules.admin.Tag.TagBizlet;
import modules.admin.domain.Tag;

import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.Message;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class ExceptTag implements ServerSideAction<Tag> {
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
		
		if(bean.getActionTag()==null){
			throw new ValidationException(new Message(Tag.actionTagPropertyName, "Select a tag to perform this action."));
		}
		
		TagBizlet.except(bean, bean.getActionTag());
		
		return new ServerSideActionResult<>(bean);
	}
}
