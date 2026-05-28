package modules.admin.Tag.actions;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
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
	 * Copies the current tag and all tagged memberships to another user.
	 *
	 * @param bean The source tag.
	 * @param webContext The current web context.
	 * @return The source tag bean.
	 * @throws Exception If validation, copy, or tagging fails.
	 */
	@Override
	public ServerSideActionResult<TagExtension> execute(TagExtension bean, WebContext webContext) throws Exception {
		if (bean.getCopyToUser() == null) {
			throw new ValidationException(new Message(Tag.copyToUserPropertyName, "You have not selected a user to copy to."));
		}
		
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
		
		webContext.growl(MessageSeverity.info, "Tag copied successfully");
		
		return new ServerSideActionResult<>(bean);
	}
}
