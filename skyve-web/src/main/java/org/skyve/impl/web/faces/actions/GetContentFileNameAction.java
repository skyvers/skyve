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

public class GetContentFileNameAction extends FacesAction<String> {

    private static final Logger FACES_LOGGER = Category.FACES.logger();

	private Bean bean;
	private String binding;
	public GetContentFileNameAction(Bean bean, String binding) {
		this.bean = bean;
		this.binding = binding;
	}
	
	@Override
	public String callback() throws Exception {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("GetContentFileNameAction - binding=" + binding);

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
