package org.skyve.web;

import java.util.Map;

import org.skyve.domain.messages.MessageSeverity;

/**
 * Push data/messages to Skyve clients.
 * @author mike
 */
public interface Pusher {
	public interface PushMessage {
		/** 
		 * For the current user
		 */
		public PushMessage user();
		
		/**
		 *  Put up a growl
		 */
		public PushMessage growl(MessageSeverity severity, String message);		
		
		/**
		 *  Put up a message
		 */
		public PushMessage message(MessageSeverity severity, String message);
		
		/**
		 *  Rerender the current view with no client validation
		 */
		public PushMessage rerender();
		
		/**
		 * Execute some javascript function
		 * @return
		 */
		public PushMessage execute(String javascriptWindowFunctionName, Map<String, Object> argumentJSON);
	}
	
	/**
	 *  Do it
	 */
	public void push(PushMessage message);
	
	/**
	 * PushMessage factory method.
	 */
	public PushMessage newPushMessage();
}
