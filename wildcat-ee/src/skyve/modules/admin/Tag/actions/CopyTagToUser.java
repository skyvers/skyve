package modules.admin.Tag.actions;

import modules.admin.domain.Tag;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

public class CopyTagToUser implements ServerSideAction<Tag> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Update the payment batch details.
	 */
	@Override
	public ServerSideActionResult execute(Tag bean, WebContext webContext) throws Exception {

		if (bean.getCopyToUser() != null) {

			// copy tag and tagged items
			Tag newTag = Util.cloneBySerialization(bean);
			newTag.setBizUserId(bean.getCopyToUser().getBizId());
			
			Persistence pers = CORE.getPersistence();
			newTag = pers.save(newTag);

			//and copy tagged items
		}
		return new ServerSideActionResult(bean);
	}
}
