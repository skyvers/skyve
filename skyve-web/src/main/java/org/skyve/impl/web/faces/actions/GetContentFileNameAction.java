package org.skyve.impl.web.faces.actions;

import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.util.Binder;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

/**
 * Executes a Faces callback action within the current Skyve web context.
 */
public class GetContentFileNameAction extends FacesAction<String> {

    private static final Logger FACES_LOGGER = Category.FACES.logger();

	private Bean bean;
	private String binding;
	
	/**
	 * Creates a content file-name lookup action for a bound content attribute.
	 *
	 * @param bean the source bean containing the content binding
	 * @param binding the binding that resolves to a content id
	 */
	public GetContentFileNameAction(Bean bean, String binding) {
		this.bean = bean;
		this.binding = binding;
	}
	
	/**
	 * Resolves the bound content id and returns its attachment file name when available.
	 *
	 * @return the attachment file name, or the empty sentinel label when no content is bound
	 * @throws Exception when content lookup fails unexpectedly
	 */
	@Override
	public String callback() throws Exception {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("GetContentFileNameAction - binding={}", binding);

		String contentId = (String) Binder.get(bean, binding);
		String fileName = "&lt;Empty&gt;";
		if (contentId != null) {
			try (ContentManager cm = EXT.newContentManager()) {
				AttachmentContent content = cm.getAttachment(contentId);
				if (content != null) {
					fileName = content.getFileName();
				}
			}
		}
		return fileName;
	}
}
