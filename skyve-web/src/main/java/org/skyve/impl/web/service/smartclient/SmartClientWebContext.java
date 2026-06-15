package org.skyve.impl.web.service.smartclient;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.web.ViewWebContext;
import org.skyve.util.Util;

import jakarta.servlet.http.HttpServletRequest;

/**
 * Captures SmartClient-specific conversation messages for later JSON rendering.
 *
 * <p>Instances are request-scoped through the owning web conversation and are not thread-safe.
 */
public final class SmartClientWebContext extends ViewWebContext {
	private static final long serialVersionUID = 7657798607012186366L;

	public static final String EDIT_ID_COUNTER = "_ecnt";
	public static final String CREATE_ID_COUNTER = "_ccnt";
	
	// lazily initialized list of growls to render
	private transient List<Map<String, String>> growls;
	// lazily initialized list of messages to render
	private transient List<Map<String, String>> messages;
	
	/**
	 * Creates a SmartClient web context for the supplied conversation key.
	 *
	 * @param key web conversation key
	 * @param request inbound HTTP request
	 */
	public SmartClientWebContext(String key, HttpServletRequest request) {
		super(key, request);
	}
	
	/**
	 * Adds a growl message for deferred SmartClient rendering.
	 *
	 * @param severity message severity
	 * @param message localized message text
	 */
	@Override
	public void growl(MessageSeverity severity, String message) {
		if (growls == null) {
			growls = new ArrayList<>();
		}
		Map<String, String> item = new TreeMap<>();
		item.put("severity", severity.toString());
		item.put("summary", Util.nullSafeI18n(message));
		growls.add(item);
	}
	
	/**
	 * Adds a standard message for deferred SmartClient rendering.
	 *
	 * @param severity message severity
	 * @param message localized message text
	 */
	@Override
	public void message(MessageSeverity severity, String message) {
		if (messages == null) {
			messages = new ArrayList<>();
		}
		Map<String, String> item = new TreeMap<>();
		item.put("severity", severity.toString());
		item.put("summary", Util.nullSafeI18n(message));
		messages.add(item);
	}
	
	/**
	 * Returns the deferred SmartClient growl messages collected so far.
	 *
	 * <p>Returns {@code null} until the first growl is recorded.
	 *
	 * @return growl message payloads, or {@code null}
	 */
	@Override
	public List<Map<String, String>> getGrowls() {
		return growls;
	}
	
	/**
	 * Returns the deferred SmartClient messages collected so far.
	 *
	 * <p>Returns {@code null} until the first message is recorded.
	 *
	 * @return message payloads, or {@code null}
	 */
	@Override
	public List<Map<String, String>> getMessages() {
		return messages;
	}
}
