package org.skyve.impl.web.service;

import static org.junit.jupiter.api.Assertions.assertTrue;
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
	void viewRendersContentUploadMetadata() throws Exception {
		String body = invokeView(u, UXUI, "kitchensink", "KitchenSink", false);

		assertTrue(body.contains("\"type\":\"content\""), body);
		assertTrue(body.contains("\"binding\":\"contentLink\""), body);
		assertTrue(body.contains("\"display\":\"link\""), body);
		assertTrue(body.contains("\"capture\":\"none\""), body);
		assertTrue(body.contains("\"binding\":\"contentImage\""), body);
		assertTrue(body.contains("\"display\":\"image\""), body);
	}

	@Test
	void viewRendersBoilerplateEscapeMetadataWithExpandedDefaults() throws Exception {
		String body = invokeView(u, UXUI, "kitchensink", "EscapingFixture", false);

		assertTrue(body.contains("\"title\":\"Escaping fixture: <i>view title</i>\",\"escapeTitle\":false"), body);
		assertTrue(body.contains("\"title\":\"Escaped <i>tab title</i>\",\"escapeTitle\":true"), body);
		assertTrue(body.contains("\"borderTitle\":\"Trusted <i>hbox border</i>\",\"escapeBorderTitle\":false"), body);
		assertTrue(body.contains("\"borderTitle\":\"Escaped <i>vbox border</i>\",\"escapeBorderTitle\":true"), body);
		assertTrue(body.contains("\"label\":\"Trusted <i>label</i>\",\"escapeLabel\":false"), body);
		assertTrue(body.contains("\"label\":\"Escaped <i>label</i>\",\"escapeLabel\":true"), body);
		assertTrue(body.contains("\"help\":\"Trusted <i>help</i>\",\"escapeHelp\":false"), body);
		assertTrue(body.contains("\"help\":\"Escaped <i>help</i>\",\"escapeHelp\":true"), body);
		assertTrue(body.contains("\"requiredMessage\":\"Trusted <i>required message</i>\",\"escapeRequiredMessage\":false"), body);
		assertTrue(body.contains("\"requiredMessage\":\"Escaped <i>required message</i>\",\"escapeRequiredMessage\":true"), body);
		assertTrue(body.contains("\"label\":\"Trusted <i>zoom display</i>\",\"escapeDisplayName\":false"), body);
		assertTrue(body.contains("\"toolTip\":\"Trusted <i>zoom tooltip</i>\",\"escapeToolTip\":false"), body);
		assertTrue(body.contains("\"value\":\"Trusted <i>link</i>\",\"escapeValue\":false"), body);
		assertTrue(body.contains("\"value\":\"Escaped <i>link</i>\",\"escapeValue\":true"), body);
		assertTrue(body.contains("\"candidatesHeading\":\"Trusted <i>candidates heading</i>\",\"escapeCandidatesHeading\":false"), body);
		assertTrue(body.contains("\"membersHeading\":\"Trusted <i>members heading</i>\",\"escapeMembersHeading\":false"), body);
		assertTrue(body.contains("\"confirm\":\"Trusted <i>cancel confirm</i>\",\"escapeConfirm\":false"), body);
		assertTrue(body.contains("\"confirm\":\"Escaped <i>save confirm</i>\",\"escapeConfirm\":true"), body);
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
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = responseWritingTo(body);

		invokeServletMethod("doGet", authenticatedViewRequest(u, UXUI, "admin", "JobSchedule"), response);

		assertTrue(body.toString().contains("\"type\":\"view\""), body.toString());
		assertTrue(body.toString().contains("\"name\":\"edit\""), body.toString());
	}

	@Test
	void doGetRendersAuthenticatedViewMetadataUsingUserAgentUxUiFallback() throws Exception {
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = responseWritingTo(body);

		invokeServletMethod("doGet", authenticatedViewRequest(u, null, "admin", "JobSchedule"), response);

		assertTrue(body.toString().contains("\"type\":\"view\""), body.toString());
		assertTrue(body.toString().contains("\"name\":\"edit\""), body.toString());
	}

	@Test
	void doGetRendersAuthenticatedViewMetadataWithForcedTopFormLabels() throws Exception {
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = responseWritingTo(body);

		invokeServletMethod("doGet", authenticatedViewRequest(u, UXUI, "admin", "JobSchedule", true), response);

		assertTrue(body.toString().contains("\"type\":\"view\""), body.toString());
		assertTrue(body.toString().contains("\"name\":\"edit\""), body.toString());
	}

	@Test
	@SuppressWarnings("static-method")
	void doGetRendersEmptyMetadataResponseWhenSessionHasEnded() throws Exception {
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = responseWritingTo(body);
		HttpServletRequest request = unauthenticatedRequest();

		invokeServletMethod("doGet", request, response);

		assertTrue(body.toString().contains("\"menus\":[]"), body.toString());
		assertTrue(body.toString().contains("\"dataSources\":[]"), body.toString());
		verify(response).setContentType("application/json");
		verify(response).setCharacterEncoding("UTF-8");
	}

	@Test
	void doGetRendersEmptyMetadataResponseWhenModuleMetadataCannotBeResolved() throws Exception {
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = responseWritingTo(body);

		invokeServletMethod("doGet", authenticatedModuleRequest(u, UXUI, "admin"), response);

		assertTrue(body.toString().contains("\"menus\":[]"), body.toString());
		assertTrue(body.toString().contains("\"dataSources\":[]"), body.toString());
	}

	@Test
	void doPostDelegatesToAuthenticatedViewMetadata() throws Exception {
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = responseWritingTo(body);

		invokeServletMethod("doPost", authenticatedViewRequest(u, UXUI, "admin", "JobSchedule"), response);

		assertTrue(body.toString().contains("\"type\":\"view\""), body.toString());
		assertTrue(body.toString().contains("\"name\":\"edit\""), body.toString());
	}

	@Test
	void doGetReturnsEmptyViewResponseWhenDocumentCannotBeResolved() throws Exception {
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = responseWritingTo(body);

		invokeServletMethod("doGet", authenticatedViewRequest(u, UXUI, "admin", "MissingDocument"), response);

		assertTrue(body.toString().contains("\"type\":\"view\""), body.toString());
		assertTrue(body.toString().contains("\"contained\":[]"), body.toString());
		assertTrue(body.toString().contains("\"title\":\"MissingDocument\""), body.toString());
	}

	private void assertViewRenders(String moduleName, String documentName) throws Exception {
		String body = invokeView(u, UXUI, moduleName, documentName, false);

		assertTrue(body.contains("\"type\":\"view\""), moduleName + '.' + documentName + ": " + body);
		assertTrue(body.contains("\"name\":\"edit\""), moduleName + '.' + documentName + ": " + body);
	}

	private static String invokeView(User user, String uxui, String moduleName, String documentName, boolean topLabels)
	throws Exception {
		Method view = metaDataServletClass().getDeclaredMethod("view",
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

	private static void invokeServletMethod(String methodName, HttpServletRequest request, HttpServletResponse response) throws Exception {
		Class<?> servletClass = metaDataServletClass();
		Object servlet = servletClass.getDeclaredConstructor().newInstance();
		Method method = servletClass.getDeclaredMethod(methodName, HttpServletRequest.class, HttpServletResponse.class);
		method.setAccessible(true);
		try {
			method.invoke(servlet, request, response);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	private static Class<?> metaDataServletClass() throws ClassNotFoundException {
		return Class.forName("org.skyve.impl.web.service.MetaDataServlet");
	}

	@SuppressWarnings("resource")
	private static HttpServletResponse responseWritingTo(java.io.StringWriter body) throws java.io.IOException {
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(new PrintWriter(body, true));
		return response;
	}

	private static HttpServletRequest authenticatedViewRequest(User user, String uxui, String moduleName, String documentName) {
		return authenticatedViewRequest(user, uxui, moduleName, documentName, false);
	}

	private static HttpServletRequest authenticatedViewRequest(User user,
																String uxui,
																String moduleName,
																String documentName,
																boolean topLabels) {
		HttpServletRequest request = authenticatedModuleRequest(user, uxui, moduleName);
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn(documentName);
		when(request.getParameter(AbstractWebContext.TOP_FORM_LABELS_NAME)).thenReturn(topLabels ? Boolean.TRUE.toString() : null);
		return request;
	}

	private static HttpServletRequest authenticatedModuleRequest(User user, String uxui, String moduleName) {
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
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn(null);
		when(request.getParameter(AbstractWebContext.TOP_FORM_LABELS_NAME)).thenReturn(null);
		return request;
	}

	private static HttpServletRequest unauthenticatedRequest() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(null);
		when(request.getUserPrincipal()).thenReturn((Principal) null);
		when(request.getLocale()).thenReturn(java.util.Locale.ENGLISH);
		when(request.getHeader("User-Agent")).thenReturn("Mozilla/5.0");
		when(request.getParameter(AbstractWebContext.UXUI)).thenReturn(UXUI);
		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn("admin");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn(null);
		return request;
	}

}
