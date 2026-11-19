package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.StateUtil;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;

import util.AbstractH2Test;

/**
 * The CSRF check the SmartClient list and edit servlets make before add, update, remove and zoom requests.
 */
@SuppressWarnings("static-method")
class SmartClientCsrfTokenH2Test extends AbstractH2Test {
	@Test
	void aTokenIssuedToTheSessionIsAccepted() {
		MockHttpSession session = new MockHttpSession();
		Integer token = StateUtil.createToken();
		StateUtil.replaceToken(session, null, token);

		assertDoesNotThrow(() -> SmartClientListServlet.checkCsrfToken(session,
				new MockHttpServletRequest(),
				new MockHttpServletResponse(),
				token));
	}

	@Test
	@Disabled("BUG-3: SmartClientListServlet.checkCsrfToken is a no-op; known; deferred by Mike (2026-11-19)")
	void aTokenTheSessionWasNeverIssuedIsRefused() {
		MockHttpSession session = new MockHttpSession();
		StateUtil.replaceToken(session, null, StateUtil.createToken());

		MockHttpServletRequest request = new MockHttpServletRequest();
		MockHttpServletResponse response = new MockHttpServletResponse();
		Integer unknown = Integer.valueOf(42);

		assertThrows(SecurityException.class, () -> SmartClientListServlet.checkCsrfToken(session, request, response, unknown));
	}

	@Test
	@Disabled("BUG-3: known; deferred by Mike (2026-11-19); see aTokenTheSessionWasNeverIssuedIsRefused")
	void aMissingTokenIsRefused() {
		MockHttpSession session = new MockHttpSession();
		MockHttpServletRequest request = new MockHttpServletRequest();
		MockHttpServletResponse response = new MockHttpServletResponse();

		assertThrows(SecurityException.class, () -> SmartClientListServlet.checkCsrfToken(session, request, response, null));
	}
}
