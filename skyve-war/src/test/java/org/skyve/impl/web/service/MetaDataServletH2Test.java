package org.skyve.impl.web.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.Principal;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.RequestUxUiSelection;
import org.skyve.impl.web.UserAgent;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import modules.test.AbstractSkyveTest;

@SuppressWarnings("java:S1192") // Repeated values are deliberate metadata rendering fixtures.
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
		assertTrue(body.contains("\"candidatesHeading\":\"Trusted <i>candidates heading</i>\",\"escapeCandidatesHeading\":false"),
				body);
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

		invokeServletMethod("doGet", authenticatedViewRequest(u, "admin", "JobSchedule"), response);

		assertTrue(body.toString().contains("\"type\":\"view\""), body.toString());
		assertTrue(body.toString().contains("\"name\":\"edit\""), body.toString());
	}

	@Test
	void doGetRendersAuthenticatedViewMetadataUsingRequestSelection() throws Exception {
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = responseWritingTo(body);

		invokeServletMethod("doGet", authenticatedViewRequest(u, "admin", "JobSchedule"), response);

		assertTrue(body.toString().contains("\"type\":\"view\""), body.toString());
		assertTrue(body.toString().contains("\"name\":\"edit\""), body.toString());
	}

	@Test
	void directMetadataRequestUsesValidatedCarriedEmulationForAccessAndRendering() throws Exception {
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = responseWritingTo(body);
		MutableRequest carried = authenticatedCarriedViewRequest(u, "phone", "admin", "JobSchedule");

		invokeServletMethod("doGet", carried.request(), response);

		assertTrue(body.toString().contains("\"type\":\"view\""), body.toString());
		RequestUxUiSelection selection = UserAgent.getSelection(carried.request());
		assertTrue(selection.isEmulated());
		assertEquals("phone", selection.getUxUi().getName());
	}

	@Test
	void doGetRendersAuthenticatedViewMetadataWithForcedTopFormLabels() throws Exception {
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = responseWritingTo(body);

		invokeServletMethod("doGet", authenticatedViewRequest(u, "admin", "JobSchedule", true), response);

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

		invokeServletMethod("doGet", authenticatedModuleRequest(u, "admin"), response);

		assertTrue(body.toString().contains("\"menus\":[]"), body.toString());
		assertTrue(body.toString().contains("\"dataSources\":[]"), body.toString());
	}

	@Test
	void doPostDelegatesToAuthenticatedViewMetadata() throws Exception {
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = responseWritingTo(body);

		invokeServletMethod("doPost", authenticatedViewRequest(u, "admin", "JobSchedule"), response);

		assertTrue(body.toString().contains("\"type\":\"view\""), body.toString());
		assertTrue(body.toString().contains("\"name\":\"edit\""), body.toString());
	}

	@Test
	void doGetReturnsEmptyViewResponseWhenDocumentCannotBeResolved() throws Exception {
		java.io.StringWriter body = new java.io.StringWriter();
		HttpServletResponse response = responseWritingTo(body);

		invokeServletMethod("doGet", authenticatedViewRequest(u, "admin", "MissingDocument"), response);

		assertTrue(body.toString().contains("\"type\":\"view\""), body.toString());
		assertTrue(body.toString().contains("\"contained\":[]"), body.toString());
		assertTrue(body.toString().contains("\"title\":\"MissingDocument\""), body.toString());
	}

	private void assertViewRenders(String moduleName, String documentName) throws Exception {
		String body = invokeView(u, UXUI, moduleName, documentName, false);

		assertTrue(body.contains("\"type\":\"view\""), moduleName + '.' + documentName + ": " + body);
		assertTrue(body.contains("\"name\":\"edit\""), moduleName + '.' + documentName + ": " + body);
	}

	@SuppressWarnings("java:S3011") // Reflection exercises the private servlet rendering seam.
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
		} catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	@SuppressWarnings({ "java:S112", "java:S3011" }) // Preserve the invoked exception while exercising the private servlet seam.
	private static void invokeServletMethod(String methodName, HttpServletRequest request, HttpServletResponse response)
			throws Exception {
		Class<?> servletClass = metaDataServletClass();
		Object servlet = servletClass.getDeclaredConstructor().newInstance();
		Method method = servletClass.getDeclaredMethod(methodName, HttpServletRequest.class, HttpServletResponse.class);
		method.setAccessible(true);
		try {
			method.invoke(servlet, request, response);
		} catch (InvocationTargetException e) {
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

	private static HttpServletRequest authenticatedViewRequest(User user, String moduleName, String documentName) {
		return authenticatedViewRequest(user, moduleName, documentName, false);
	}

	private static HttpServletRequest authenticatedViewRequest(User user,
			String moduleName,
			String documentName,
			boolean topLabels) {
		HttpServletRequest request = authenticatedModuleRequest(user, moduleName);
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn(documentName);
		when(request.getParameter(AbstractWebContext.TOP_FORM_LABELS_NAME)).thenReturn(topLabels ? Boolean.TRUE.toString() : null);
		return request;
	}

	private static HttpServletRequest authenticatedModuleRequest(User user, String moduleName) {
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
		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn("admin");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn(null);
		return request;
	}

	private static MutableRequest authenticatedCarriedViewRequest(User user,
			String carriedType,
			String moduleName,
			String documentName) {
		HttpSession session = mock(HttpSession.class);
		when(session.getId()).thenReturn("metadata-carried-session-" + System.nanoTime());
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
		StateUtil.addSession(user.getId(), session);

		Map<String, Object> attributes = new HashMap<>();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession()).thenReturn(session);
		when(request.getSession(false)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn((Principal) null);
		when(request.getLocale()).thenReturn(java.util.Locale.ENGLISH);
		when(request.getHeader("User-Agent")).thenReturn("Mozilla/5.0");
		when(request.getAttribute(any())).thenAnswer(invocation -> attributes.get(invocation.getArgument(0)));
		when(session.getAttribute(AbstractWebContext.EMULATED_USER_AGENT_TYPE_SESSION_ATTRIBUTE_NAME))
				.thenReturn(org.skyve.web.UserAgentType.valueOf(carriedType));
		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn(moduleName);
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn(documentName);
		when(request.getParameter(AbstractWebContext.TOP_FORM_LABELS_NAME)).thenReturn(null);
		org.mockito.Mockito.doAnswer(invocation -> {
			attributes.put(invocation.getArgument(0), invocation.getArgument(1));
			return null;
		}).when(request).setAttribute(any(), any());
		org.mockito.Mockito.doAnswer(invocation -> {
			attributes.remove(invocation.getArgument(0));
			return null;
		}).when(request).removeAttribute(any());
		return new MutableRequest(request, attributes);
	}

	private record MutableRequest(HttpServletRequest request, Map<String, Object> attributes) {
		// Immutable test carrier.
	}

}
