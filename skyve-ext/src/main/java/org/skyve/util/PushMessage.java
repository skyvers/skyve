package org.skyve.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.skyve.CORE;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.websocket.Session;

/**
 * Use this class to specify a push message to send to connected client user interfaces.
 * By default the message will be broadcast to all currently connected users of the system.
 * Use the {@link #user()} and overloaded variants to constrain the message to a subset of connected users.
 * If a user is specified that is not currently using the system, the push message is not sent.
 */
public class PushMessage {
	private static final String ITEM_TYPE = "type";
	private static final String ITEM_SEVERITY = "severity";
	private static final String ITEM_MESSAGE = "message";
	private static final String ITEM_METHOD = "method";
	private static final String ITEM_ARGUMENT = "argument";
	
	/**
	 * Holds the connected clients.
	 */
	public static final ConcurrentLinkedQueue<Session> SESSIONS = new ConcurrentLinkedQueue<>();

	private Set<String> userIds = new TreeSet<>();
	private List<Map<String, Object>> items = new ArrayList<>();

	/** 
	 * For the current user
	 */
	public @Nonnull PushMessage user() {
		userIds.add(CORE.getUser().getId());
		return this;
	}
	
	/**
	 * For another user
	 */
	public @Nonnull PushMessage user(@Nonnull String userId) {
		userIds.add(userId);
		return this;
	}
	
	/**
	 * For another user
	 */
	public @Nonnull PushMessage user(@Nonnull User user) {
		userIds.add(user.getId());
		return this;
	}
	
	/**
	 * Get the user IDs this message will go to.
	 */
	public @Nonnull Set<String> getUserIds() {
		return userIds;
	}
	
	/**
	 * Get the items in this push message.
	 * @return A set of JSON command objects such as growl, message, rerender, execute destined for a connected client.
	 */
	public @Nonnull List<Map<String, Object>> getItems() {
		return items;
	}

	/**
	 *  Put up a growl
	 */
	public @Nonnull PushMessage growl(@Nonnull MessageSeverity severity, @Nonnull String message) {
		Map<String, Object> item = new TreeMap<>();
		item.put(ITEM_TYPE, "g");
		item.put(ITEM_SEVERITY, severity.toString());
		item.put(ITEM_MESSAGE, Util.nullSafeI18n(message));
		items.add(item);
		return this;
	}
	
	/**
	 *  Put up a message
	 */
	public @Nonnull PushMessage message(@Nonnull MessageSeverity severity, @Nonnull String message) {
		Map<String, Object> item = new TreeMap<>();
		item.put(ITEM_TYPE, "m");
		item.put(ITEM_SEVERITY, severity.toString());
		item.put(ITEM_MESSAGE, Util.nullSafeI18n(message));
		items.add(item);
		return this;
	}

	/**
	 *  Rerender the current view with no client validation
	 */
	public @Nonnull PushMessage rerender() {
		Map<String, Object> item = new TreeMap<>();
		item.put(ITEM_TYPE, "r");
		items.add(item);
		return this;
	}

	/**
	 * Execute some javascript function
	 */
	public @Nonnull PushMessage execute(@Nonnull String javascriptWindowFunctionName,
											@Nonnull Map<String, Object> argumentJSON) {
		Map<String, Object> item = new TreeMap<>();
		item.put(ITEM_TYPE, "j");
		item.put(ITEM_METHOD, javascriptWindowFunctionName);
		item.put(ITEM_ARGUMENT, argumentJSON);
		items.add(item);
		return this;
	}
}
