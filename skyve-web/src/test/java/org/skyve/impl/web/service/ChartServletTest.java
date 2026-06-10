package org.skyve.impl.web.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.Locale;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.impl.persistence.AbstractPersistence;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings("static-method")
class ChartServletTest {
	@AfterEach
	void tearDown() throws Exception {
		clearThreadPersistence();
	}

	@Test
	void doGetReturnsEmptyJsonWhenRequestHasNoAuthenticatedUser() throws Exception {
		AbstractPersistence persistence = bindThreadPersistence();
		ChartServlet servlet = new ChartServlet();
		HttpServletRequest request = requestWithoutUser();
		StringWriter body = new StringWriter();
		HttpServletResponse response = responseWritingTo(body);

		servlet.doGet(request, response);

		assertEquals("{}", body.toString());
		verify(response).setContentType(MimeType.json.toString());
		verify(response).setCharacterEncoding(StandardCharsets.UTF_8.name());
		verify(response).addHeader("Cache-control", "private,no-cache,no-store");
		verify(response).addDateHeader("Expires", 0);
		verify(persistence).begin();
		verify(persistence).rollback();
		verify(persistence).commit(true);
	}

	@Test
	void doPostDelegatesToDoGetFallback() throws Exception {
		AbstractPersistence persistence = bindThreadPersistence();
		ChartServlet servlet = new ChartServlet();
		HttpServletRequest request = requestWithoutUser();
		StringWriter body = new StringWriter();
		HttpServletResponse response = responseWritingTo(body);

		servlet.doPost(request, response);

		assertEquals("{}", body.toString());
		verify(persistence).begin();
		verify(persistence).rollback();
		verify(persistence).commit(true);
	}

	private static HttpServletRequest requestWithoutUser() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(null);
		when(request.getUserPrincipal()).thenReturn(null);
		when(request.getHeader("Authorization")).thenReturn(null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		return request;
	}

	private static HttpServletResponse responseWritingTo(StringWriter body) throws Exception {
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(new PrintWriter(body));
		return response;
	}

	private static AbstractPersistence bindThreadPersistence() {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setForThread();
		return persistence;
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
