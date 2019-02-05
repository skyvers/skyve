package org.skyve.web;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.web.Pusher.PushMessage;

/**
 * 
 */
public interface WebContext {
	/**
	 * The name of the web session attribute representing the logged in user.
	 */
	public static final String USER_SESSION_ATTRIBUTE_NAME = "user";

	/**
	 * 
	 * @return
	 */
	public String getKey();
	
	/**
	 * 
	 * @param key
	 */
	public void setKey(String key);
	
	/**
	 * 
	 * @param bizId
	 * @return
	 */
	public Bean getBean(String bizId);
	
	/**
	 * 
	 * @return
	 */
	public Bean getCurrentBean();
	
	/**
	 * 
	 * @param currentBean
	 */
	public void setCurrentBean(Bean currentBean);
	
	/**
	 * The context key and the current bizId smashed together.
	 * @return
	 */
	public String getWebId();
	
	/**
	 * 
	 * @return
	 */
	public String getAction();
	
	/**
	 * 
	 * @param action
	 */
	public void setAction(String action);
	
	/**
	 * The web request to process - This is transient, not part of the encoded context.
	 */
	public Object getHttpServletRequest();
	public void setHttpServletRequest(Object request);
	
	/**
	 * The web response to generate - This is transient, not part of the encoded context.
	 */
	public Object getHttpServletResponse();
	public void setHttpServletResponse(Object response);

	// TODO - implement view push/pop/replace/parent refresh
	// This class should have methods to accomplish the following
	// push a new view - ie popup on client-side, or render on server-side stack
	// pop a view off
	// get an action to be able to refresh its parent (what about refresh the parent's parent)
	// get a view to change its binding?
	// does an edit view needs its list view as a parent?
	// should the state of the views (ie history) be available for server-side interrogation?
	
	/**
	 * PushMessage factory method
	 * @return	A new push message.
	 */
	public PushMessage newPushMessage();
	
	/**
	 * This method is used to push to clients.
	 * @param message	The message to push.
	 */
	public void push(PushMessage message) throws Exception;
	
	/**
	 * Add a message to the current view to be displayed.
	 * @param severity
	 * @param message
	 */
	public void message(MessageSeverity severity, String message);
	
	/**
	 * Add a growl (toast) to the current view to be popped.
	 * @param severity
	 * @param message
	 */
	public void growl(MessageSeverity severity, String message);
}
