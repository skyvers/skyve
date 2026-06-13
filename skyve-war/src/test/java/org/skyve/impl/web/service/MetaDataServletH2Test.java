package org.skyve.impl.web.service;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
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
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import modules.test.AbstractSkyveTest;

class MetaDataServletH2Test extends AbstractSkyveTest {
	private static final String UXUI = "external";

	@AfterEach
	void restorePersistenceUser() {
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void viewRendersRealEditViewMetadata() throws Exception {
		assertViewRenders("admin", "JobSchedule");
		assertViewRenders("admin", "ReportTemplate");
		assertViewRenders("admin", "Startup");
		assertViewRenders("admin", "UserAccount");
		assertViewRenders("admin", "MonitoringDashboard");
		assertViewRenders("admin", "ControlPanel");
		assertViewRenders("admin", "Tag");
		assertViewRenders("admin", "Communication");
		assertViewRenders("admin", "ImportExport");
		assertViewRenders("admin", "DataMaintenance");
		assertViewRenders("admin", "User");
		assertViewRenders("admin", "UserList");
		assertViewRenders("kitchensink", "KitchenSink");
	}

	@Test
	void viewRendersWithForcedTopFormLabels() throws Exception {
		String body = invokeView(u, UXUI, "admin", "JobSchedule", true);

		assertTrue(body.contains("\"type\":\"view\""), body);
		assertTrue(body.contains("\"name\":\"edit\""), body);
	}

	@Test
	void viewRendersDesktopEditViewMetadata() throws Exception {
		String body = invokeView(u, "desktop", "admin", "JobSchedule", false);

		assertTrue(body.contains("\"type\":\"view\""), body);
		assertTrue(body.contains("\"name\":\"edit\""), body);
	}

	@Test
	void doGetRendersAuthenticatedViewMetadata() throws Exception {
		MetaDataServlet servlet = new MetaDataServlet();
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(new PrintWriter(body, true));

		servlet.doGet(authenticatedViewRequest(u, UXUI, "admin", "JobSchedule"), response);

		assertTrue(body.toString().contains("\"type\":\"view\""), body.toString());
		assertTrue(body.toString().contains("\"name\":\"edit\""), body.toString());
	}

	private void assertViewRenders(String moduleName, String documentName) throws Exception {
		String body = invokeView(u, UXUI, moduleName, documentName, false);

		assertTrue(body.contains("\"type\":\"view\""), moduleName + '.' + documentName + ": " + body);
		assertTrue(body.contains("\"name\":\"edit\""), moduleName + '.' + documentName + ": " + body);
	}

	private static String invokeView(User user, String uxui, String moduleName, String documentName, boolean topLabels)
	throws Exception {
		Method view = MetaDataServlet.class.getDeclaredMethod("view",
				User.class,
				String.class,
				String.class,
				String.class,
				boolean.class);
		view.setAccessible(true);
		try {
			return view.invoke(null, user, uxui, moduleName, documentName, Boolean.valueOf(topLabels)).toString();
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	private static HttpServletRequest authenticatedViewRequest(User user, String uxui, String moduleName, String documentName) {
		HttpSession session = mock(HttpSession.class);
		when(session.getId()).thenReturn("metadata-session-" + System.nanoTime());
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
		StateUtil.addSession(user.getId(), session);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession()).thenReturn(session);
		when(request.getSession(false)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn((Principal) null);
		when(request.getLocale()).thenReturn(java.util.Locale.ENGLISH);
		when(request.getHeader("User-Agent")).thenReturn("Mozilla/5.0");
		when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(null);
		when(request.getParameter(AbstractWebContext.UXUI)).thenReturn(uxui);
		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn(moduleName);
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn(documentName);
		when(request.getParameter(AbstractWebContext.TOP_FORM_LABELS_NAME)).thenReturn(null);
		return request;
	}

}
