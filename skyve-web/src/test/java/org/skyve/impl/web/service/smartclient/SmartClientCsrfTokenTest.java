package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.web.service.smartclient.SmartClientListServlet.CsrfRefusedException;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;

/**
 * The session's CSRF token, which every SmartClient response carries in its header and which the list, edit,
 * snapshot and tag servlets check before a change, answering a refusal by sending the page to the home page.
 */
@SuppressWarnings("static-method")
class SmartClientCsrfTokenTest {
	@Test
	void theSessionsTokenIsAccepted() {
		MockHttpServletRequest request = post(new MockHttpSession());
		String token = StateUtil.getSessionToken(request.getSession());

		assertDoesNotThrow(() -> SmartClientListServlet.checkCsrfToken(request, token));
	}

	@Test
	void aMaskOfTheSessionsTokenIsAccepted() {
		MockHttpServletRequest request = post(new MockHttpSession());
		String mask = StateUtil.maskToken(StateUtil.getSessionToken(request.getSession()));

		assertDoesNotThrow(() -> SmartClientListServlet.checkCsrfToken(request, mask));
	}

	@Test
	void aChangeThatIsNotAPostIsRefusedEvenWithTheSessionsToken() {
		MockHttpSession session = new MockHttpSession();
		String token = StateUtil.getSessionToken(session);

		for (String method : new String[] {"GET", "PUT", "DELETE"}) {
			MockHttpServletRequest request = new MockHttpServletRequest(method, "/smartedit");
			request.setSession(session);
			assertThrows(CsrfRefusedException.class, () -> SmartClientListServlet.checkCsrfToken(request, token), method);
		}
	}

	@Test
	void anotherSessionsTokenIsRefused() {
		MockHttpServletRequest request = post(new MockHttpSession());
		StateUtil.getSessionToken(request.getSession());
		String otherSessions = StateUtil.getSessionToken(new MockHttpSession());

		assertThrows(CsrfRefusedException.class, () -> SmartClientListServlet.checkCsrfToken(request, otherSessions));
	}

	@Test
	void aMissingTokenIsRefused() {
		MockHttpServletRequest request = post(new MockHttpSession());
		StateUtil.getSessionToken(request.getSession());

		assertThrows(CsrfRefusedException.class, () -> SmartClientListServlet.checkCsrfToken(request, null));
	}

	@Test
	void aSessionThatWasNeverIssuedATokenRefusesEveryToken() {
		MockHttpSession session = new MockHttpSession();
		MockHttpServletRequest request = post(session);

		assertThrows(CsrfRefusedException.class, () -> SmartClientListServlet.checkCsrfToken(request, ""));
		assertFalse(session.getAttributeNames().hasMoreElements(), "checking issues no token");
	}

	@Test
	void aRequestWithoutASessionIsRefused() {
		MockHttpServletRequest request = new MockHttpServletRequest("POST", "/smartedit");
		String token = StateUtil.createToken();

		assertThrows(CsrfRefusedException.class, () -> SmartClientListServlet.checkCsrfToken(request, token));
	}

	@Test
	void aRefusalTellsThePageToReloadAndOtherFailuresDoNot() {
		StringWriter refused = new StringWriter();
		StringWriter failed = new StringWriter();

		assertTrue(SmartClientListServlet.produceCsrfRefusedResponse(new CsrfRefusedException("refused"), new PrintWriter(refused, true)));
		assertFalse(SmartClientListServlet.produceCsrfRefusedResponse(new SecurityException("other"), new PrintWriter(failed, true)));

		assertEquals(SmartClientListServlet.LOGIN_REQUIRED_MARKER, refused.toString());
		assertEquals("", failed.toString(), "another failure is answered by the servlet's own error response");
	}

	@Test
	void everyResponseOfASessionCarriesTheSameToken() {
		MockHttpServletRequest request = post(new MockHttpSession());
		MockHttpServletResponse first = new MockHttpServletResponse();
		MockHttpServletResponse second = new MockHttpServletResponse();

		SmartClientListServlet.setCsrfTokenHeader(request, first);
		SmartClientListServlet.setCsrfTokenHeader(request, second);

		assertEquals(StateUtil.getSessionToken(request.getSession()), first.getHeader("X-CSRF-TOKEN"));
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

	private static MockHttpServletRequest post(MockHttpSession session) {
		MockHttpServletRequest result = new MockHttpServletRequest("POST", "/smartedit");
		result.setSession(session);
		return result;
	}
}
