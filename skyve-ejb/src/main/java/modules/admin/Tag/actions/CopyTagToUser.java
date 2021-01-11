package modules.admin.Tag.actions;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.domain.Tag;

public class CopyTagToUser implements ServerSideAction<Tag> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Update the payment batch details.
	 */
	@Override
	public ServerSideActionResult<Tag> execute(Tag bean, WebContext webContext) throws Exception {

		if (bean.getCopyToUser() != null) {

			// copy tag and tagged items
			Tag newTag = Tag.newInstance();
			newTag.setName(bean.getName());
			newTag.setBizUserId(bean.getCopyToUser().getBizId());
			Persistence pers = CORE.getPersistence();
			pers.upsertBeanTuple(newTag);
			
			EXT.tag(newTag.getBizId(), EXT.iterateTagged(bean.getBizId()));
		}
		return new ServerSideActionResult<>(bean);
	}
}
