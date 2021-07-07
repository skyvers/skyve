package org.skyve.impl.web;

import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import org.skyve.impl.cache.StateUtil;

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
