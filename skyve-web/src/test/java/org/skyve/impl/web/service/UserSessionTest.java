package org.skyve.impl.web.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.skyve.impl.metadata.user.ClientUserData;

@SuppressWarnings("static-method")
public class UserSessionTest {

	@Test
	public void getSessionIdReturnsNullInitially() {
		UserSession session = new UserSession();
		assertNull(session.getSessionID());
	}

	@Test
	public void setAndGetSessionIdRoundtrip() {
		UserSession session = new UserSession();
		session.setSessionID("abc123");
		assertEquals("abc123", session.getSessionID());
	}

	@Test
	public void getClientUserDataReturnsNullInitially() {
		UserSession session = new UserSession();
		assertNull(session.getClientUserData());
	}

	@Test
	public void setAndGetClientUserDataRoundtrip() {
		UserSession session = new UserSession();
		ClientUserData data = new ClientUserData();
		session.setClientUserData(data);
		assertEquals(data, session.getClientUserData());
	}
}
