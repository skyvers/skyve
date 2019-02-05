package org.skyve.impl.web.faces.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.enterprise.context.ApplicationScoped;
import javax.enterprise.inject.Default;
import javax.inject.Inject;
import javax.inject.Named;

import org.omnifaces.cdi.Push;
import org.omnifaces.cdi.PushContext;
import org.skyve.CORE;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.web.Pusher;

@Named
@Default
@ApplicationScoped
public class FacesPusher implements Pusher, Serializable {
	private static final long serialVersionUID = 4597126077286549454L;

	@Inject
	@Push(channel = "skyve")
	private PushContext push;

	@Override
	public synchronized void push(PushMessage message) {
		FacesPushMessage m = (FacesPushMessage) message;
		if (m.isUser()) {
			push.send(m.getItems(), CORE.getUser().getId());
		}
		else {
			push.send(m.getItems());
		}
	}
	
	@Override
	public PushMessage newPushMessage() {
		return new FacesPushMessage();
	}
	
	public static class FacesPushMessage implements PushMessage {
		private static final String ITEM_TYPE = "type";
		private static final String ITEM_SEVERITY = "severity";
		private static final String ITEM_MESSAGE = "message";
		private static final String ITEM_METHOD = "method";
		private static final String ITEM_ARGUMENT = "argument";
		
		private boolean user = false;
		
		private List<Map<String, Object>> items = new ArrayList<>();

		/** 
		 * For the current user
		 */
		@Override
		public PushMessage user() {
			user = true;
			return this;
		}
		
		boolean isUser() {
			return user;
		}
		
		/**
		 *  Put up a growl
		 */
		@Override
		public PushMessage growl(MessageSeverity severity, String message) {
			Map<String, Object> item = new TreeMap<>();
			item.put(ITEM_TYPE, "g");
			item.put(ITEM_SEVERITY, severity.toString());
			item.put(ITEM_MESSAGE, message);
			items.add(item);
			return this;
		}
		
		/**
		 *  Put up a message
		 */
		@Override
		public PushMessage message(MessageSeverity severity, String message) {
			Map<String, Object> item = new TreeMap<>();
			item.put(ITEM_TYPE, "m");
			item.put(ITEM_SEVERITY, severity.toString());
			item.put(ITEM_MESSAGE, message);
			items.add(item);
			return this;
		}
	
		/**
		 *  Rerender the current view with no client validation
		 */
		@Override
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
		@Override
		public PushMessage execute(String javascriptWindowFunctionName, Map<String, Object> argumentJSON) {
			Map<String, Object> item = new TreeMap<>();
			item.put(ITEM_TYPE, "j");
			item.put(ITEM_METHOD, javascriptWindowFunctionName);
			item.put(ITEM_ARGUMENT, argumentJSON);
			items.add(item);
			return this;
		}
		
		public List<Map<String, Object>> getItems() {
			return items;
		}
	}
}
