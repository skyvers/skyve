package org.skyve.impl.web.faces;

import org.omnifaces.cdi.push.SocketEndpoint;
import org.skyve.util.PushMessage;

import jakarta.websocket.CloseReason;
import jakarta.websocket.EndpointConfig;
import jakarta.websocket.Session;

/**
 * This class extends omnifaces socket endpoint to enable the collection of 
 * web socket sessions so we can use the same socket for user pushes and broadcast pushes.
 * The class is registered against the container in SkyveContextListener programmatically.
 */
public class SkyveSocketEndpoint extends SocketEndpoint {
	@Override
	public void onOpen(Session session, EndpointConfig config) {
		super.onOpen(session, config);
		PushMessage.SESSIONS.add(session);
//		Util.LOGGER.info("WebSocket connection opened for user " + session.getUserProperties().get("user"));
	}

	@Override
	public void onClose(Session session, CloseReason reason) {
		super.onClose(session, reason);
		PushMessage.SESSIONS.remove(session);
//		Util.LOGGER.info("WebSocket connection closed for user " + session.getUserProperties().get("user"));
	}
}
