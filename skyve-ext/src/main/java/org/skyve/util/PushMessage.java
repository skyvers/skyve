package org.skyve.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentLinkedQueue;

import javax.websocket.Session;

import org.skyve.CORE;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.user.User;

public class PushMessage {
	private static final String ITEM_TYPE = "type";
	private static final String ITEM_SEVERITY = "severity";
	private static final String ITEM_MESSAGE = "message";
	private static final String ITEM_METHOD = "method";
	private static final String ITEM_ARGUMENT = "argument";
	
	public static final ConcurrentLinkedQueue<Session> SESSIONS = new ConcurrentLinkedQueue<>();

	private Set<String> userIds = new TreeSet<>();
	private List<Map<String, Object>> items = new ArrayList<>();

	/** 
	 * For the current user
	 */
	public PushMessage user() {
		userIds.add(CORE.getUser().getId());
		return this;
	}
	
	/**
	 * For another user
	 */
	public PushMessage user(String userId) {
		userIds.add(userId);
		return this;
	}
	
	public PushMessage user(User user) {
		userIds.add(user.getId());
		return this;
	}
	
	public Set<String> getUserIds() {
		return userIds;
	}
	
	public List<Map<String, Object>> getItems() {
		return items;
	}

	/**
	 *  Put up a growl
	 */
	public PushMessage growl(MessageSeverity severity, String message) {
		Map<String, Object> item = new TreeMap<>();
		item.put(ITEM_TYPE, "g");
		item.put(ITEM_SEVERITY, severity.toString());
		item.put(ITEM_MESSAGE, Util.i18n(message));
		items.add(item);
		return this;
	}
	
	/**
	 *  Put up a message
	 */
	public PushMessage message(MessageSeverity severity, String message) {
		Map<String, Object> item = new TreeMap<>();
		item.put(ITEM_TYPE, "m");
		item.put(ITEM_SEVERITY, severity.toString());
		item.put(ITEM_MESSAGE, Util.i18n(message));
		items.add(item);
		return this;
	}

	/**
	 *  Rerender the current view with no client validation
	 */
	public PushMessage rerender() {
		Map<String, Object> item = new TreeMap<>();
		item.put(ITEM_TYPE, "r");
		items.add(item);
		return this;
	}

	/**
	 * Execute some javascript function
	 * @return
	 */
	public PushMessage execute(String javascriptWindowFunctionName, Map<String, Object> argumentJSON) {
		Map<String, Object> item = new TreeMap<>();
		item.put(ITEM_TYPE, "j");
		item.put(ITEM_METHOD, javascriptWindowFunctionName);
		item.put(ITEM_ARGUMENT, argumentJSON);
		items.add(item);
		return this;
	}
}
