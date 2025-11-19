package modules.admin.Tag.actions;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.Persistence;
import org.skyve.tag.TagManager;
import org.skyve.web.WebContext;

import modules.admin.Tag.TagExtension;
import modules.admin.domain.Tag;

/**
 * Server-side action that copies a tag and all its tagged items to another user.
 * Creates a new tag with the same name under the target user and duplicates
 * all item associations from the original tag.
 */
public class CopyTagToUser implements ServerSideAction<TagExtension> {
	/**
	 * Copy tag to another user.
	 */
	@Override
	public ServerSideActionResult<TagExtension> execute(TagExtension bean, WebContext webContext) throws Exception {
		if (bean.getCopyToUser() != null) {
			// copy tag and tagged items
			Tag newTag = Tag.newInstance();
			newTag.setName(bean.getName());
			newTag.setBizUserId(bean.getCopyToUser().getBizId());
			Persistence pers = CORE.getPersistence();
			pers.upsertBeanTuple(newTag);

			TagManager tm = EXT.getTagManager();
			try (AutoClosingIterable<Bean> i = tm.iterate(bean.getBizId())) {
				tm.tag(newTag.getBizId(), i);
			}
		}
		return new ServerSideActionResult<>(bean);
	}
}
