package org.skyve.impl.web.service;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.security.Principal;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.web.WebContext;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import modules.test.AbstractSkyveTest;

@SuppressWarnings("resource")
class MapServletH2Test extends AbstractSkyveTest {
	@AfterEach
	void restorePersistenceUser() {
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void doGetReturnsEmptyMapResultWhenNoConversationBeanIsAvailable() throws Exception {
		StringWriter body = new StringWriter();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(new PrintWriter(body, true));

		new TestableMapServlet().doGet(authenticatedRequest(), response);

		String json = body.toString();
		assertTrue(json.contains("\"items\":null"), json);
		assertTrue(json.contains("\"mapExtents\":null"), json);
		verify(response).setContentType(MimeType.json.toString());
		verify(response).setCharacterEncoding(java.nio.charset.StandardCharsets.UTF_8.name());
		verify(response).addHeader("Cache-control", "private,no-cache,no-store");
		verify(response).addDateHeader("Expires", 0);
	}

	private HttpServletRequest authenticatedRequest() {
		HttpSession session = mock(HttpSession.class);
		when(session.getId()).thenReturn("map-session-" + System.nanoTime());
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(u);
		StateUtil.addSession(u.getId(), session);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession()).thenReturn(session);
		when(request.getSession(false)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn((Principal) null);
		when(request.getLocale()).thenReturn(java.util.Locale.ENGLISH);
		when(request.getHeader("User-Agent")).thenReturn("Mozilla/5.0");
		when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(null);
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn(null);
		return request;
	}

	private static final class TestableMapServlet extends MapServlet {
		private static final long serialVersionUID = 1L;

		@Override
		public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
			super.doGet(request, response);
		}
	}
}
