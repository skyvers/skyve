package org.skyve.impl.web.service;

import org.skyve.impl.metadata.user.ClientUserData;

/**
 * Encapsulates a User's session.
 * User - their menus etc and document permissions
 * Customer - Default converters etc
 * Modules - ???
 * 
 * @author Mike
 */
public final class UserSession {
	private String sessionID;
	private ClientUserData clientUserData;

	/**
	 * Returns the server-side HTTP session identifier.
	 *
	 * @return session identifier
	 */
	public String getSessionID() {
		return sessionID;
	}

	/**
	 * Sets the server-side HTTP session identifier.
	 *
	 * @param sessionID session identifier
	 */
	public void setSessionID(String sessionID) {
		this.sessionID = sessionID;
	}

	/**
	 * Returns cached user metadata associated with the session.
	 *
	 * @return client user data
	 */
	public ClientUserData getClientUserData() {
		return clientUserData;
	}

	/**
	 * Sets cached user metadata associated with the session.
	 *
	 * @param clientUserData client user data
	 */
	public void setClientUserData(ClientUserData clientUserData) {
		this.clientUserData = clientUserData;
	}
}
