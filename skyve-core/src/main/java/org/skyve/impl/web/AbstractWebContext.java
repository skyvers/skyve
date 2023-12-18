package org.skyve.impl.web;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.web.WebContext;

public abstract class AbstractWebContext implements Serializable, WebContext {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 876761059493617411L;
	
	/**
	 * The name of the request parameter which holds the encoded context on the web client.
	 */
	public static final String CONTEXT_NAME = "_c";
	public static final String CONTINUE_CONVERSATION = "_cc";
	public static final String DOCUMENT_NAME = "_doc";
	public static final String MODULE_NAME = "_mod";
	public static final String BEAN_NAME = "_bean";
	public static final String ID_NAME = "_id";
	public static final String MODEL_NAME = "_m";
	public static final String BINDING_NAME = "_b";
	public static final String GRID_BINDING_NAME = "_g";
	public static final String QUERY_NAME = "_q";
	public static final String ACTION_NAME = "_a";
	public static final String SOURCE_NAME = "_s";
	public static final String CUSTOMER_COOKIE_NAME = "customer";
	public static final String REPORT_NAME = "_n";
	public static final String REPORT_FORMAT = "_f";
	public static final String REPORT_ENGINE = "_e";
	public static final String RESOURCE_FILE_NAME = "_n";
	public static final String CURRENT_TIME_IN_MILLIS = "_ctim";
	public static final String IS_LIST = "_list";
	public static final String CSRF_TOKEN_NAME = "_csrf";

	/**
	 *  Used to place the uxui (renderer) at play during the request as a request attribute
	 *  or in the session when switched in the UI.
	 */
	public static final String UXUI = "skyveUxUi";

	/**
	 * Used to place the user agent type of the requesting device as a request attribute.
	 */
	public static String USER_AGENT_TYPE_KEY = "skyveUserAgentType";

	/**
	 * Used to indicate whether the user agent type was emulated or detected.
	 */
	public static String EMULATED_USER_AGENT_TYPE_KEY = "skyveEnumlatedUserAgentType";

	private Map<String, Bean> contextBeans = new TreeMap<>();

	private AbstractPersistence conversation;

	// the key to use to place this conversation context into the session
	private transient String key;
	
	private transient String action;

	private transient Bean currentBean;
	
	/**
	 * The web request to process - This is transient, not part of the encoded context.
	 */
	private transient Object request;
	
	/**
	 * The web response to generate - This is transient, not part of the encoded context.
	 */
	private transient Object response;
	
	protected AbstractWebContext(String key, Object request, Object response) {
		this.key = key;
		this.request = request;
		this.response = response;
	}
	
	@Override
	public String getKey() {
		return key;
	}
	
	@Override
	public void setKey(String key) {
		this.key = key;
	}
	
	@Override
	public final Bean getBean(String bizId) {
		return contextBeans.get(bizId);
	}

	@Override
	public final Bean getCurrentBean() {
		return currentBean;
	}
	
	@Override
	public final void setCurrentBean(Bean currentBean) {
		if (currentBean == null) {
			if (this.currentBean != null) {
				contextBeans.remove(this.currentBean.getBizId());
			}
		}
		else {
			contextBeans.put(currentBean.getBizId(), currentBean);
		}
		this.currentBean = currentBean;
	}
	
	/**
	 * The context key and the current bizId smashed together.
	 * @return
	 */
	@Override
	public final String getWebId() {
		if (currentBean == null) {
			return key;
		}
		
		return key + currentBean.getBizId();
	}
	
	@Override
	public final String getAction() {
		return action;
	}

	@Override
	public final void setAction(String action) {
		this.action = action;
	}
	
	public AbstractPersistence getConversation() {
		return conversation;
	}

	public void setConversation(AbstractPersistence conversation) {
		this.conversation = conversation;
	}
	
	@Override
	public final Object getHttpServletRequest() {
		return request;
	}
	@Override
	public final void setHttpServletRequest(Object request) {
		this.request = request;
	}

	@Override
	public final Object getHttpServletResponse() {
		return response;
	}
	@Override
	public final void setHttpServletResponse(Object response) {
		this.response = response;
	}

	/**
	 * Get a list of accumulated growls to display.
	 * @return	<code>null</code> or [{severity:'info/warn/error/fatal', summary: 'message'}...]
	 */
	public abstract List<Map<String, String>> getGrowls();

	/**
	 * Get a list of accumulated messages to display.
	 * @return	<code>null</code> or [{severity:'info/warn/error/fatal', summary: 'message'}...]
	 */
	public abstract List<Map<String, String>> getMessages();
	
	@Override
	public String toString() {
		return super.toString() + '#' + getWebId();
	}
	
	// TODO - implement view push/pop/replace/parent refresh
	// This class should have methods to accomplish the following
	// push a new view - ie popup on client-side, or render on server-side stack
	// pop a view off
	// get an action to be able to refresh its parent (what about refresh the parent's parent)
	// get a view to change its binding?
	// does an edit view needs its list view as a parent?
	// should the state of the views (ie history) be available for server-side interrogation?
}
