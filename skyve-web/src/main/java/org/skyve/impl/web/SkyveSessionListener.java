package org.skyve.impl.web;

import org.skyve.impl.cache.StateUtil;

import jakarta.servlet.http.HttpSessionEvent;
import jakarta.servlet.http.HttpSessionListener;

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
		StateUtil.incrementSessionCount();
	}

	@Override
	public void sessionDestroyed(HttpSessionEvent se) {
		StateUtil.decrementSessionCount();
	}
}
