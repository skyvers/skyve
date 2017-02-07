package modules.admin.Tag.actions;

import modules.admin.Tag.TagBizlet;
import modules.admin.domain.Tag;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class PerformCombination implements ServerSideAction<Tag> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Update the payment batch details.
	 */
	@Override
	public ServerSideActionResult<Tag> execute(Tag bean, WebContext webContext) throws Exception {

		if (bean.getCombinationsOperator() == null) {
			throw new ValidationException(new Message(Tag.combinationsOperatorPropertyName, "You have not set an operator."));
		}

		if (bean.getActionTag() == null) {
			throw new ValidationException(new Message(Tag.actionTagPropertyName, "You have not set the other Tag."));
		}

		switch (bean.getCombinationsOperator()) {
		case union:
			TagBizlet.union(bean, bean.getActionTag());
			break;
		case except:
			TagBizlet.except(bean, bean.getActionTag());
			break;
		case intersect:
			TagBizlet.intersect(bean, bean.getActionTag());
			break;
		default:
			break;
		}
		
		//recount
		bean.setCurrentTagCount(TagBizlet.getCount(bean));
		bean.setActionTagCount(TagBizlet.getCount(bean.getActionTag()));


		return new ServerSideActionResult<>(bean);
	}
}
