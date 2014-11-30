package org.skyve.wildcat.web.service.smartclient;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.primefaces.push.EventBusFactory;
import org.skyve.wildcat.web.AbstractWebContext;

public final class SmartClientWebContext extends AbstractWebContext {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 7657798607012186366L;

	public static final String EDIT_ID_COUNTER = "_ecnt";
	public static final String CREATE_ID_COUNTER = "_ccnt";
	
	public SmartClientWebContext(String key, 
									HttpServletRequest request, 
									HttpServletResponse response) {
		super(key, request, response);
	}
	
	@Override
	public void push(String path, Object o) {
		EventBusFactory.getDefault().eventBus().publish(path, o);
	}
}
