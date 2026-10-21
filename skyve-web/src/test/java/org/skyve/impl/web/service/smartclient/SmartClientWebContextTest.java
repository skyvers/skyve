package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Locale;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.SessionEndedException;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings("static-method")
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

	@Test
	void preparedDownloadMatchesOnlyItsDocumentActionAndTarget() throws Exception {
		SmartClientWebContext context = context();
		assertFalse(context.isPreparedDownload("mod.Doc", "Download", null, null));
		context.recordPreparedDownload("Download", "mod", "Doc", "child", "rows", "row1");

		assertTrue(context.isPreparedDownload("mod.Doc", "Download", "child.rows", "row1"));
		assertTrue(context.isPreparedDownload("mod.Doc", "Download", "child.rows", "row1"));
		assertFalse(context.isPreparedDownload("mod.Other", "Download", "child.rows", "row1"));
		assertFalse(context.isPreparedDownload("mod.Doc", "Other", "child.rows", "row1"));
		assertFalse(context.isPreparedDownload("mod.Doc", "Download", "rows", "row1"));
		assertFalse(context.isPreparedDownload("mod.Doc", "Download", "child.rows", "row2"));
	}

	@Test
	void preparedDownloadSurvivesConversationSerializationAndIsReplaced() throws Exception {
		SmartClientWebContext context = context();
		context.recordPreparedDownload("First", "mod", "Doc", null, null, "ignored");
		context.recordPreparedDownload("Second", "mod", "Doc", null, "rows", "row1");

		ByteArrayOutputStream bytes = new ByteArrayOutputStream();
		try (ObjectOutputStream out = new ObjectOutputStream(bytes)) {
			out.writeObject(context);
		}
		SmartClientWebContext restored;
		try (ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray()))) {
			restored = (SmartClientWebContext) in.readObject();
		}

		assertFalse(restored.isPreparedDownload("mod.Doc", "First", null, null));
		assertTrue(restored.isPreparedDownload("mod.Doc", "Second", "rows", "row1"));
	}

	private static SmartClientWebContext context() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getSession(false)).thenReturn(session);
		when(session.getId()).thenReturn("session-1");
		return new SmartClientWebContext("ctx", request);
	}
}
