package org.skyve.impl.web;

import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import org.skyve.cache.ConversationUtil;

/**
 * Used to count the active sessions on a server.
 * The session count is on ConversationUtil as this is in skyve-ext, not skyve-web 
 * and so is available to all application tiers
 * 
 * @author mike
 */
public class SkyveSessionListener implements HttpSessionListener {
	@Override
	public void sessionCreated(HttpSessionEvent se) {
		ConversationUtil.incrementSessionCount();
	}

	@Override
	public void sessionDestroyed(HttpSessionEvent se) {
		ConversationUtil.incrementSessionCount();
	}
}
