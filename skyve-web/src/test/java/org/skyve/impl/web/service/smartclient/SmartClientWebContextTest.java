package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Locale;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.SessionEndedException;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

class SmartClientWebContextTest {
	@Test
	void growlAndMessageInitialiseCollectionsAndStoreEntries() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getSession(false)).thenReturn(session);
		when(session.getId()).thenReturn("session-1");

		SmartClientWebContext context = new SmartClientWebContext("ctx", request);
		assertNull(context.getGrowls());
		assertNull(context.getMessages());

		context.growl(MessageSeverity.info, "hello");
		context.message(MessageSeverity.warn, "careful");
		context.growl(MessageSeverity.error, "boom");
		context.message(MessageSeverity.fatal, "stop");

		assertNotNull(context.getGrowls());
		assertNotNull(context.getMessages());
		assertEquals(2, context.getGrowls().size());
		assertEquals(2, context.getMessages().size());

		Map<String, String> growl = context.getGrowls().get(0);
		assertEquals("info", growl.get("severity"));
		assertEquals("hello", growl.get("summary"));

		Map<String, String> message = context.getMessages().get(0);
		assertEquals("warn", message.get("severity"));
		assertEquals("careful", message.get("summary"));

		Map<String, String> secondGrowl = context.getGrowls().get(1);
		assertEquals("error", secondGrowl.get("severity"));
		assertEquals("boom", secondGrowl.get("summary"));

		Map<String, String> secondMessage = context.getMessages().get(1);
		assertEquals("fatal", secondMessage.get("severity"));
		assertEquals("stop", secondMessage.get("summary"));
	}

	@Test
	void constructorThrowsWhenSessionMissing() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);

		assertThrows(SessionEndedException.class, () -> new SmartClientWebContext("ctx", request));
	}
}
