package org.skyve.impl.web.faces.actions;

import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.util.Binder;
import org.skyve.util.Util;

public class GetResourceURLAction extends FacesAction<String> {
	private Bean bean;
	private String binding;
	public GetResourceURLAction(Bean bean, String binding) {
		this.bean = bean;
		this.binding = binding;
	}
	
	@Override
	public String callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("GetResourceURLAction - binding=" + binding);

		String contentId = (String) Binder.get(bean, binding);
		if (contentId == null) {
			return "images/blank.gif";
		}

		StringBuilder result = new StringBuilder(128);
		result.append("content?").append(AbstractWebContext.RESOURCE_FILE_NAME).append('=').append(contentId);
		result.append('&').append(AbstractWebContext.DOCUMENT_NAME).append('=').append(bean.getBizModule()).append('.').append(bean.getBizDocument());
		result.append('&').append(AbstractWebContext.BINDING_NAME).append('=').append(binding);
		result.append('&').append(AbstractWebContext.CURRENT_TIME_IN_MILLIS).append('=').append(System.currentTimeMillis());
		
		return result.toString();
	}
}
