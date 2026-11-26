package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.StateUtil;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;

/**
 * The session's CSRF token, which every SmartClient response carries in its header and which the list, edit,
 * snapshot and tag servlets check before a mutating request.
 */
@SuppressWarnings("static-method")
class SmartClientCsrfTokenTest {
	@Test
	void theSessionsTokenIsAccepted() {
		MockHttpSession session = new MockHttpSession();
		String token = StateUtil.getSessionToken(session);

		assertDoesNotThrow(() -> SmartClientListServlet.checkCsrfToken(session, token));
	}

	@Test
	void anotherSessionsTokenIsRefused() {
		MockHttpSession session = new MockHttpSession();
		StateUtil.getSessionToken(session);
		String otherSessions = StateUtil.getSessionToken(new MockHttpSession());

		assertThrows(SecurityException.class, () -> SmartClientListServlet.checkCsrfToken(session, otherSessions));
	}

	@Test
	void aMissingTokenIsRefused() {
		MockHttpSession session = new MockHttpSession();
		StateUtil.getSessionToken(session);

		assertThrows(SecurityException.class, () -> SmartClientListServlet.checkCsrfToken(session, null));
	}

	@Test
	void aSessionThatWasNeverIssuedATokenRefusesEveryToken() {
		MockHttpSession session = new MockHttpSession();

		assertThrows(SecurityException.class, () -> SmartClientListServlet.checkCsrfToken(session, ""));
		assertFalse(session.getAttributeNames().hasMoreElements(), "checking issues no token");
	}

	@Test
	void everyResponseOfASessionCarriesTheSameToken() {
		MockHttpServletRequest request = new MockHttpServletRequest();
		MockHttpSession session = new MockHttpSession();
		request.setSession(session);
		MockHttpServletResponse first = new MockHttpServletResponse();
		MockHttpServletResponse second = new MockHttpServletResponse();

		SmartClientListServlet.setCsrfTokenHeader(request, first);
		SmartClientListServlet.setCsrfTokenHeader(request, second);

		assertEquals(StateUtil.getSessionToken(session), first.getHeader("X-CSRF-TOKEN"));
		assertEquals(first.getHeader("X-CSRF-TOKEN"), second.getHeader("X-CSRF-TOKEN"));
	}

	@Test
	void aRequestWithoutASessionGetsNoTokenHeader() {
		MockHttpServletRequest request = new MockHttpServletRequest();
		MockHttpServletResponse response = new MockHttpServletResponse();

		SmartClientListServlet.setCsrfTokenHeader(request, response);

		assertNull(response.getHeader("X-CSRF-TOKEN"));
		assertNull(request.getSession(false), "no session is created");
	}
}
