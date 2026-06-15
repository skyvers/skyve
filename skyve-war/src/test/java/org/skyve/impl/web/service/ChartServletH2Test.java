package org.skyve.impl.web.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.Principal;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.content.MimeType;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import jakarta.servlet.http.HttpServletRequest;
import modules.test.AbstractSkyveTest;

@SuppressWarnings({"static-method", "resource"})
class ChartServletH2Test extends AbstractSkyveTest {
	@AfterEach
	void restorePersistenceUser() {
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void processListModelReturnsEmptyResponseForModelDatasource() throws Exception {
		String config = invokeProcessListModel(chartRequest("test_AllAttributesPersistent__SomeModel", null, null));

		assertEquals("{}", config);
	}

	@Test
	void processListModelBuildsEmptyResponseBeforeReadingChartParametersForModelDatasource() throws Exception {
		String config = invokeProcessListModel(chartRequest("test_AllAttributesPersistent__AnotherModel", "not-json", "not-a-chart"));

		assertEquals("{}", config);
	}

	@Test
	void doGetReturnsEmptyJsonForAuthenticatedModelDatasource() throws Exception {
		ChartServlet servlet = new ChartServlet();
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(new PrintWriter(body, true));

		servlet.doGet(authenticatedChartRequest(u, "test_AllAttributesPersistent__SomeModel"), response);

		assertEquals("{}", body.toString());
		verify(response).setContentType(MimeType.json.toString());
		verify(response).setCharacterEncoding(java.nio.charset.StandardCharsets.UTF_8.name());
		verify(response).addHeader("Cache-control", "private,no-cache,no-store");
		verify(response).addDateHeader("Expires", 0);
	}

	@Test
	void doPostDelegatesToAuthenticatedModelDatasourceResponse() throws Exception {
		ChartServlet servlet = new ChartServlet();
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(new PrintWriter(body, true));

		servlet.doPost(authenticatedChartRequest(u, "test_AllAttributesPersistent__SomeModel"), response);

		assertEquals("{}", body.toString());
	}

	@Test
	void doGetReturnsEmptyJsonWhenDatasourceIsMissingAndConversationHasNoBean() throws Exception {
		ChartServlet servlet = new ChartServlet();
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(new PrintWriter(body, true));

		servlet.doGet(authenticatedChartRequest(u, null), response);

		assertEquals("{}", body.toString());
	}

	@Test
	void doGetReturnsEmptyJsonWhenDatasourceCannotBeParsed() throws Exception {
		ChartServlet servlet = new ChartServlet();
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(new PrintWriter(body, true));

		servlet.doGet(authenticatedChartRequest(u, "malformed"), response);

		assertEquals("{}", body.toString());
	}

	private static HttpServletRequest chartRequest(String dataSourceName, String criteriaJson, String builderJson) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameter("ds")).thenReturn(dataSourceName);
		when(request.getParameter("criteria")).thenReturn(criteriaJson);
		when(request.getParameter("b")).thenReturn(builderJson);
		when(request.getParameter("t")).thenReturn("bar");
		return request;
	}

	private static HttpServletRequest authenticatedChartRequest(User user, String dataSourceName) {
		HttpSession session = mock(HttpSession.class);
		when(session.getId()).thenReturn("chart-session-" + System.nanoTime());
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
		StateUtil.addSession(user.getId(), session);

		HttpServletRequest request = chartRequest(dataSourceName, null, null);
		when(request.getSession()).thenReturn(session);
		when(request.getSession(false)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn((Principal) null);
		when(request.getLocale()).thenReturn(java.util.Locale.ENGLISH);
		when(request.getHeader("User-Agent")).thenReturn("Mozilla/5.0");
		when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(null);
		return request;
	}

	private static String invokeProcessListModel(HttpServletRequest request) throws Exception {
		Method processListModel = ChartServlet.class.getDeclaredMethod("processListModel", HttpServletRequest.class);
		processListModel.setAccessible(true);
		try {
			return (String) processListModel.invoke(null, request);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

}
