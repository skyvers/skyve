package org.skyve.impl.sail.mock;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Locale;

import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.mock.web.MockServletContext;

import jakarta.servlet.http.Cookie;

@SuppressWarnings({ "static-method", "java:S1192" }) // Repeated values form the shared map-delegation fixture vocabulary.
class MockExternalContextTest {
	@Test
	void mutableStateIsRetained() {
		MockExternalContext context = new MockExternalContext();

		context.getApplicationMap().put("application", "value");
		context.getInitParameterMap().put("initial", "value");
		context.getRequestCookieMap().put("cookie", "value");
		context.getRequestHeaderMap().put("header", "value");
		context.getRequestHeaderValuesMap().put("headers", new String[] {"one", "two"});
		context.getRequestMap().put("request", "value");
		context.getRequestParameterMap().put("parameter", "value");
		context.getRequestParameterValuesMap().put("parameters", new String[] {"one", "two"});
		context.getSessionMap().put("session", "value");
		context.setRequestLocale(Locale.CANADA_FRENCH);

		assertEquals("value", context.getApplicationMap().get("application"));
		assertEquals("value", context.getInitParameter("initial"));
		assertEquals("value", context.getRequestCookieMap().get("cookie"));
		assertEquals("value", context.getRequestHeaderMap().get("header"));
		assertEquals("two", context.getRequestHeaderValuesMap().get("headers")[1]);
		assertEquals("value", context.getRequestMap().get("request"));
		assertEquals("value", context.getRequestParameterMap().get("parameter"));
		assertEquals("parameter", context.getRequestParameterNames().next());
		assertEquals("two", context.getRequestParameterValuesMap().get("parameters")[1]);
		assertEquals("value", context.getSessionMap().get("session"));
		assertNull(context.getRequest());
		assertNull(context.getResponse());
		assertEquals(Locale.CANADA_FRENCH, context.getRequestLocale());
		assertEquals(Locale.CANADA_FRENCH, context.getRequestLocales().next());
	}

	@Test
	@SuppressWarnings("resource") // The container-independent context returns no stream.
	void containerIndependentDefaultsAreSafe() throws Exception {
		MockExternalContext context = new MockExternalContext();

		assertEquals(Locale.getDefault(), context.getRequestLocale());
		assertEquals("action", context.encodeActionURL("action"));
		assertEquals("namespace", context.encodeNamespace("namespace"));
		assertEquals("resource", context.encodeResourceURL("resource"));
		assertEquals("socket", context.encodeWebsocketURL("socket"));
		assertNull(context.getAuthType());
		assertNull(context.getContext());
		assertNull(context.getRemoteUser());
		assertNull(context.getRequestContextPath());
		assertNull(context.getRequestPathInfo());
		assertNull(context.getRequestServletPath());
		assertNull(context.getResource("resource"));
		assertNull(context.getResourceAsStream("resource"));
		assertFalse(context.getResourcePaths("resource").iterator().hasNext());
		assertNull(context.getSession(true));
		assertNull(context.getUserPrincipal());
		assertFalse(context.isUserInRole("role"));

		context.dispatch("path");
		context.log("message");
		context.log("message", new RuntimeException());
		context.redirect("url");
		context.release();
	}

	@Test
	@SuppressWarnings("java:S5961") // one fixture checked through every view the external context offers onto it
	void servletObjectsBackExternalContextState() throws Exception {
		MockServletContext servletContext = new MockServletContext();
		MockHttpServletRequest request = new MockHttpServletRequest(servletContext);
		MockHttpServletResponse response = new MockHttpServletResponse();
		MockHttpSession session = new MockHttpSession(servletContext);
		Cookie cookie = new Cookie("cookie", "value");

		servletContext.setInitParameter("initial", "value");
		servletContext.setAttribute("application", "value");
		request.setAttribute("attribute", "value");
		request.setCookies(cookie);
		request.addHeader("X-Test", "one");
		request.addHeader("X-Test", "two");
		request.setPreferredLocales(java.util.List.of(Locale.UK, Locale.CANADA));
		request.setParameter("parameter", "one", "two");
		request.setScheme("https");
		request.setServerName("example.test");
		request.setServerPort(8443);
		request.setSecure(true);
		request.setContentType("text/plain");
		request.setContent("content".getBytes(java.nio.charset.StandardCharsets.UTF_8));
		request.setSession(session);
		session.setAttribute("session", "value");

		MockExternalContext context = new MockExternalContext();
		context.setRequest(request);
		context.setResponse(response);

		assertSame(request, context.getRequest());
		assertSame(response, context.getResponse());
		assertSame(servletContext, context.getContext());
		assertEquals("value", context.getInitParameter("initial"));
		assertEquals("value", context.getApplicationMap().get("application"));
		assertEquals("value", context.getRequestMap().get("attribute"));
		assertSame(cookie, context.getRequestCookieMap().get("cookie"));
		assertEquals("one", context.getRequestHeaderMap().get("X-Test"));
		assertEquals("two", context.getRequestHeaderValuesMap().get("X-Test")[1]);
		assertEquals(Locale.UK, context.getRequestLocale());
		assertEquals("one", context.getRequestParameterMap().get("parameter"));
		assertEquals("two", context.getRequestParameterValuesMap().get("parameter")[1]);
		assertEquals("value", context.getSessionMap().get("session"));
		assertEquals("https", context.getRequestScheme());
		assertEquals("example.test", context.getRequestServerName());
		assertEquals(8443, context.getRequestServerPort());
		assertEquals("text/plain", context.getRequestContentType());
		assertEquals(7, context.getRequestContentLength());
		assertTrue(context.isSecure());

		context.getApplicationMap().put("newApplication", "newValue");
		context.getRequestMap().put("newRequest", "newValue");
		context.getSessionMap().put("newSession", "newValue");
		assertEquals("newValue", servletContext.getAttribute("newApplication"));
		assertEquals("newValue", request.getAttribute("newRequest"));
		assertEquals("newValue", session.getAttribute("newSession"));

		context.setResponseCharacterEncoding("UTF-8");
		context.setResponseContentType("application/json");
		context.setResponseHeader("X-Set", "one");
		context.addResponseHeader("X-Set", "two");
		context.setResponseStatus(201);
		context.addResponseCookie("result", "value", java.util.Map.of("httpOnly", Boolean.TRUE));
		assertEquals("UTF-8", response.getCharacterEncoding());
		assertTrue(response.getContentType().startsWith("application/json"));
		assertEquals(response.getContentType(), context.getResponseContentType());
		assertEquals(java.util.List.of("one", "two"), response.getHeaders("X-Set"));
		assertEquals(201, response.getStatus());
		assertTrue(response.getCookie("result").isHttpOnly());

		context.redirect("/target");
		assertEquals("/target", response.getRedirectedUrl());
	}
}
