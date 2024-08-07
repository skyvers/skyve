package org.skyve.impl.web.service.smartclient;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.web.ViewWebContext;
import org.skyve.util.Util;

import jakarta.servlet.http.HttpServletRequest;

public final class SmartClientWebContext extends ViewWebContext {
	private static final long serialVersionUID = 7657798607012186366L;

	public static final String EDIT_ID_COUNTER = "_ecnt";
	public static final String CREATE_ID_COUNTER = "_ccnt";
	
	// lazily initialized list of growls to render
	private transient List<Map<String, String>> growls;
	// lazily initialized list of messages to render
	private transient List<Map<String, String>> messages;
	
	public SmartClientWebContext(String key, 
			HttpServletRequest request) {
		super(key, request);
	}
	
	@Override
	public void growl(MessageSeverity severity, String message) {
		if (growls == null) {
			growls = new ArrayList<>();
		}
		Map<String, String> item = new TreeMap<>();
		item.put("severity", severity.toString());
		item.put("summary", Util.i18n(message));
		growls.add(item);
	}
	
	@Override
	public void message(MessageSeverity severity, String message) {
		if (messages == null) {
			messages = new ArrayList<>();
		}
		Map<String, String> item = new TreeMap<>();
		item.put("severity", severity.toString());
		item.put("summary", Util.i18n(message));
		messages.add(item);
	}
	
	@Override
	public List<Map<String, String>> getGrowls() {
		return growls;
	}
	
	@Override
	public List<Map<String, String>> getMessages() {
		return messages;
	}
}
