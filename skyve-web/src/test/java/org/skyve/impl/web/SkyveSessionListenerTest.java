package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.StateUtil;

import jakarta.servlet.http.HttpSession;
import jakarta.servlet.http.HttpSessionEvent;

@SuppressWarnings("static-method")
class SkyveSessionListenerTest {
	@BeforeEach
	void resetSessionCount() {
		while (StateUtil.getSessionCount() > 0) {
			StateUtil.decrementSessionCount();
		}
	}

	@Test
	void sessionCreatedIncrementsCount() {
		SkyveSessionListener listener = new SkyveSessionListener();
		HttpSession session = mock(HttpSession.class);
		HttpSessionEvent event = new HttpSessionEvent(session);

		assertEquals(0, StateUtil.getSessionCount());
		listener.sessionCreated(event);
		assertEquals(1, StateUtil.getSessionCount());
	}

	@Test
	void sessionDestroyedWithoutSkyveUserDecrementsAndClampsToZero() {
		SkyveSessionListener listener = new SkyveSessionListener();
		HttpSession session = mock(HttpSession.class);
		when(session.getAttribute(org.skyve.web.WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		HttpSessionEvent event = new HttpSessionEvent(session);

		listener.sessionDestroyed(event);

		assertEquals(0, StateUtil.getSessionCount());
	}
}
