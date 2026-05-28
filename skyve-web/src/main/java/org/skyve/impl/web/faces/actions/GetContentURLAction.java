package org.skyve.impl.web.faces.actions;

import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.util.Binder;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

/**
 * Executes a Faces callback action within the current Skyve web context.
 */
public class GetContentURLAction extends FacesAction<String> {

    private static final Logger FACES_LOGGER = Category.FACES.logger();

	private Bean bean;
	private String binding;
	private boolean image;
	
	/**
	 * Creates a content URL resolver for a specific bean binding.
	 *
	 * @param bean the bean containing the content reference
	 * @param binding the binding path to the content id field
	 * @param image whether the URL is for an image resource fallback
	 */
	public GetContentURLAction(Bean bean, String binding, boolean image) {
		this.bean = bean;
		this.binding = binding;
		this.image = image;
	}
	
	/**
	 * Resolves the URL for the bound content reference.
	 *
	 * <p>When no content id exists, this returns an image-safe placeholder URL for image fields
	 * and a no-op JavaScript URL for non-image content links.
	 *
	 * @return a relative content URL, a blank image URL, or a no-op URL depending on content presence
	 * @throws Exception if binding resolution fails
	 */
	@Override
	public String callback() throws Exception {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("GetContentURLAction - binding={}", binding);

		String contentId = (String) Binder.get(bean, binding);
		if (contentId == null) {
			if (image) {
				return "images/blank.gif";
			}
			return "javascript:void(0)";
		}

		StringBuilder result = new StringBuilder(128);
		result.append("content?").append(AbstractWebContext.RESOURCE_FILE_NAME).append('=').append(contentId);
		result.append('&').append(AbstractWebContext.DOCUMENT_NAME).append('=').append(bean.getBizModule()).append('.').append(bean.getBizDocument());
		result.append('&').append(AbstractWebContext.BINDING_NAME).append('=').append(binding);
		
		return result.toString();
	}
}
