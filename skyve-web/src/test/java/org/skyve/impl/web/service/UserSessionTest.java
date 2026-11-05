package org.skyve.impl.web.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.user.ClientUserData;

@SuppressWarnings("static-method")
class UserSessionTest {

	@Test
	void getSessionIdReturnsNullInitially() {
		UserSession session = new UserSession();
		assertNull(session.getSessionID());
	}

	@Test
	void setAndGetSessionIdRoundtrip() {
		UserSession session = new UserSession();
		session.setSessionID("abc123");
		assertEquals("abc123", session.getSessionID());
	}

	@Test
	void getClientUserDataReturnsNullInitially() {
		UserSession session = new UserSession();
		assertNull(session.getClientUserData());
	}

	@Test
	void setAndGetClientUserDataRoundtrip() {
		UserSession session = new UserSession();
		ClientUserData data = new ClientUserData();
		session.setClientUserData(data);
		assertEquals(data, session.getClientUserData());
	}
}
