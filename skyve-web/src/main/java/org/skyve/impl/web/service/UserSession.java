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
public final class UserSession
{
	private String sessionID;
	private ClientUserData clientUserData;

	public String getSessionID() {
		return sessionID;
	}
	public void setSessionID(String sessionID) {
		this.sessionID = sessionID;
	}
	public ClientUserData getClientUserData() {
		return clientUserData;
	}
	public void setClientUserData(ClientUserData clientUserData) {
		this.clientUserData = clientUserData;
	}
}
