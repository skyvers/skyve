package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.RETURNS_SELF;
import static org.mockito.Mockito.when;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.security.Principal;
import java.util.Locale;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.View;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import jakarta.servlet.ServletConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings({"java:S5786", "static-method", "resource"})
class SmartClientGeneratorServletTest {
	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();

	@AfterEach
	void tearDown() throws Exception {
		Field rendererClass = SmartClientGeneratorServlet.class.getDeclaredField("RENDERER_CLASS");
		rendererClass.setAccessible(true);
		rendererClass.set(null, null);
		ProvidedRepositoryFactory.set(originalRepository);
		clearThreadPersistence();
	}

	@Test
	void newRendererUsesDefaultImplementationWhenNoOverrideConfigured() {
		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(mock(User.class),
				new ModuleImpl(),
				new DocumentImpl(),
				new ViewImpl(),
				"desktop",
				false);

		assertNotNull(renderer);
		assertInstanceOf(SmartClientViewRenderer.class, renderer);
	}

	@Test
	void initLoadsConfiguredRendererClassAndNewRendererUsesIt() throws Exception {
		ServletConfig config = mock(ServletConfig.class);
		when(config.getInitParameter("renderer")).thenReturn(TestRenderer.class.getName());

		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		servlet.init(config);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(mock(User.class),
				new ModuleImpl(),
				new DocumentImpl(),
				new ViewImpl(),
				"desktop",
				true);

		assertInstanceOf(TestRenderer.class, renderer);
	}

	@Test
	void initRejectsInvalidRendererClass() {
		ServletConfig config = mock(ServletConfig.class);
		when(config.getInitParameter("renderer")).thenReturn("no.such.Renderer");

		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		assertThrows(ServletException.class, () -> servlet.init(config));
	}

	@Test
	void initAcceptsRendererClassWithWrongTypeButNewRendererFails() throws Exception {
		ServletConfig config = mock(ServletConfig.class);
		when(config.getInitParameter("renderer")).thenReturn(String.class.getName());

		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		servlet.init(config);

		assertThrows(DomainException.class,
				() -> SmartClientGeneratorServlet.newRenderer(mock(User.class),
						new ModuleImpl(),
						new DocumentImpl(),
						new ViewImpl(),
						"desktop",
						false));
	}

	@Test
	void initIgnoresBlankRendererParameter() throws Exception {
		ServletConfig config = mock(ServletConfig.class);
		when(config.getInitParameter("renderer")).thenReturn("   ");

		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		servlet.init(config);

		SmartClientViewRenderer renderer = SmartClientGeneratorServlet.newRenderer(mock(User.class),
				new ModuleImpl(),
				new DocumentImpl(),
				new ViewImpl(),
				"desktop",
				false);
		assertInstanceOf(SmartClientViewRenderer.class, renderer);
	}

	@Test
	void newRendererDoesNotThrowWhenOverrideClassIsReset() {
		assertDoesNotThrow(() -> SmartClientGeneratorServlet.newRenderer(mock(User.class),
				new ModuleImpl(),
				new DocumentImpl(),
				new ViewImpl(),
				"desktop",
				false));
	}

	@Test
	void newRendererWrapsReflectionFailureWhenConstructorSignatureDoesNotMatch() throws Exception {
		ServletConfig config = mock(ServletConfig.class);
		when(config.getInitParameter("renderer")).thenReturn(BadCtorRenderer.class.getName());

		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		servlet.init(config);

		assertThrows(DomainException.class,
				() -> SmartClientGeneratorServlet.newRenderer(mock(User.class),
						new ModuleImpl(),
						new DocumentImpl(),
						new ViewImpl(),
						"desktop",
						false));
	}

	@Test
	void doGetWarnsWhenModuleNameIsMissing() throws Exception {
		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn(null);
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn("Contact");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetWarnsWhenDocumentNameIsMissing() throws Exception {
		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn("admin");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn(null);

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetWarnsWhenModuleNameIsBlank() throws Exception {
		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn("   ");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn("Contact");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetWarnsWhenDocumentNameIsBlank() throws Exception {
		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn("admin");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn("   ");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetWarnsWhenUserCannotBeResolved() throws Exception {
		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		HttpServletRequest request = newRequestWithoutUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn("admin");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn("Contact");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void appendUnexpectedWarningIncludesFailurePrefix() {
		StringWriter sink = new StringWriter();
		PrintWriter writer = new PrintWriter(sink);

		SmartClientGeneratorServlet.appendUnexpectedWarning("REF-35", writer);
		writer.flush();

		org.junit.jupiter.api.Assertions.assertTrue(sink.toString().contains("Could not generate views."));
	}

	@Test
	void doGetWarnsWhenAccessIsDenied() throws Exception {
		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn("admin");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn("Contact");
		when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(UxUi.newPrimeFaces("desktop", "template", "saga"));

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	private static HttpServletRequest newRequestWithSessionUser() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		UserImpl user = new UserImpl();
		user.setId("apiUserId");
		user.setCustomerName("demo");
		user.setName("apiUser");
		when(repository.retrieveUser("apiUser")).thenReturn(user);
		ProvidedRepositoryFactory.set(repository);

		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		Principal principal = mock(Principal.class);
		when(principal.getName()).thenReturn("apiUser");
		when(request.getSession(false)).thenReturn(session);
		when(request.getSession(true)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn(principal);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(session.getId()).thenReturn("session-1");
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		return request;
	}

	private static HttpServletRequest newRequestWithoutUser() {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setForThread();

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getSession(false)).thenReturn(session);
		when(request.getSession(true)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn(null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		return request;
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}

	public static final class TestRenderer extends SmartClientViewRenderer {
		public TestRenderer(User user, Module module, Document document, View view, String uxui, Boolean noCreateView) {
			super(user, module, document, view, uxui, noCreateView.booleanValue());
		}
	}

	public static final class BadCtorRenderer extends SmartClientViewRenderer {
		public BadCtorRenderer() {
			super(null, new ModuleImpl(), new DocumentImpl(), new ViewImpl(), "desktop", false);
		}
	}
}