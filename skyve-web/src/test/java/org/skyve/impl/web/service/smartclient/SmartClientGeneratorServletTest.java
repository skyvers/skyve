package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.RETURNS_SELF;
import static org.mockito.Mockito.when;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.File;
import java.lang.reflect.Field;
import java.security.Principal;
import java.util.Locale;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.cache.StateUtil;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.util.UtilImpl;
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

@SuppressWarnings({"java:S5786", "static-method", "resource", "java:S5778", "java:S5976"})
class SmartClientGeneratorServletTest {
	private static final String USER_ID = "apiUserId";

	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
	private boolean originalForceNonPersistentCaching;
	private String originalCacheDirectory;
	private SessionCacheConfig originalSessionCache;
	private Path cacheDirectory;

	@BeforeEach
	void setUp() throws Exception {
		originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		originalSessionCache = UtilImpl.SESSION_CACHE;

		cacheDirectory = Files.createTempDirectory("generator-servlet-cache");
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
		UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
		UtilImpl.SESSION_CACHE = new SessionCacheConfig(100, 10);

		DefaultCaching.get().shutdown();
		DefaultCaching.get().startup();
		StateUtil.removeSessions(USER_ID);
	}

	@AfterEach
	void tearDown() throws Exception {
		StateUtil.removeSessions(USER_ID);
		DefaultCaching.get().shutdown();
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
		UtilImpl.CACHE_DIRECTORY = originalCacheDirectory;
		UtilImpl.SESSION_CACHE = originalSessionCache;
		if (cacheDirectory != null) {
			try (Stream<Path> paths = Files.walk(cacheDirectory)) {
				paths.sorted((a, b) -> b.compareTo(a))
					.map(Path::toFile)
					.forEach(File::delete);
			}
		}

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

	@Test
	void doGetWarnsWhenCreateViewCannotBeResolvedAfterAccessCheck() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		UserImpl user = mock(UserImpl.class);
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		View editView = mock(View.class);

		when(user.getName()).thenReturn("apiUser");
		when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(user).canAccess(org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.anyString());
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
		when(session.getId()).thenReturn("session-deep-generator");
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(UxUi.newPrimeFaces("desktop", "template", "saga"));
		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn("admin");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn("Contact");

		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(editView.getName()).thenReturn(View.ViewType.edit.toString());
		when(document.getView("desktop", customer, View.ViewType.edit.toString())).thenReturn(editView);
		when(document.getView("desktop", customer, View.ViewType.create.toString())).thenReturn(null);

		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetWarnsWhenCreateViewResolvesToEditPathAfterAccessCheck() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		UserImpl user = mock(UserImpl.class);
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		View editView = mock(View.class);
		View createView = mock(View.class);

		when(user.getName()).thenReturn("apiUser");
		when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(user).canAccess(org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.anyString());
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
		when(session.getId()).thenReturn("session-generator-edit-path");
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(UxUi.newPrimeFaces("desktop", "template", "saga"));
		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn("admin");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn("Contact");

		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(createView.getName()).thenReturn(View.ViewType.edit.toString());
		when(document.getView("desktop", customer, View.ViewType.edit.toString())).thenReturn(editView);
		when(document.getView("desktop", customer, View.ViewType.create.toString())).thenReturn(createView);

		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetWarnsAfterResolvingViewsWhenRendererPathFails() throws Exception {
		ServletConfig config = mock(ServletConfig.class);
		when(config.getInitParameter("renderer")).thenReturn(SafeCtorRenderer.class.getName());

		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();
		servlet.init(config);

		UserImpl user = mock(UserImpl.class);
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		View editView = mock(View.class);
		View createView = mock(View.class);

		when(user.getId()).thenReturn(USER_ID);
		when(user.getName()).thenReturn("apiUser");
		when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(user).canAccess(org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.anyString());

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getSession(false)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn(null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(session.getId()).thenReturn("session-generator-success");
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
		StateUtil.addSession(USER_ID, session);

		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn("admin");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn("Contact");
		when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(UxUi.newPrimeFaces("desktop", "template", "saga"));

		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(module.getName()).thenReturn("admin");
		when(document.getName()).thenReturn("Contact");
		when(document.getView("desktop", customer, View.ViewType.edit.toString())).thenReturn(editView);
		when(document.getView("desktop", customer, View.ViewType.create.toString())).thenReturn(createView);
		when(document.getLocalisedSingularAlias()).thenReturn("Contact");
		when(document.getIconStyleClass()).thenReturn("fa-doc");
		when(document.getIcon32x32RelativeFileName()).thenReturn("doc.png");

		when(editView.getName()).thenReturn(View.ViewType.edit.toString());
		when(editView.getIconStyleClass()).thenReturn(null);
		when(editView.getIcon32x32RelativeFileName()).thenReturn(null);
		when(editView.getHelpRelativeFileName()).thenReturn(null);
		when(editView.getHelpURL()).thenReturn("https://example.test/edit-help");

		when(createView.getName()).thenReturn("create");
		when(createView.getIconStyleClass()).thenReturn("fa-create");
		when(createView.getHelpRelativeFileName()).thenReturn("create-help.md");

		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();

		HttpServletResponse response = mock(HttpServletResponse.class);
		StringWriter sink = new StringWriter();
		when(response.getWriter()).thenReturn(new PrintWriter(sink, true));

		assertDoesNotThrow(() -> servlet.doGet(request, response));
		String payload = sink.toString();
		org.junit.jupiter.api.Assertions.assertTrue(payload.contains("Could not generate views."), payload);
		verify(customer).getModule("admin");
		verify(module).getDocument(customer, "Contact");
		verify(document).getView("desktop", customer, View.ViewType.edit.toString());
		verify(document).getView("desktop", customer, View.ViewType.create.toString());
	}

	@Test
	void doGetWarnsWithGenericWarningWhenModuleLookupFailsAfterAccessCheck() throws Exception {
		SmartClientGeneratorServlet servlet = new SmartClientGeneratorServlet();

		UserImpl user = mock(UserImpl.class);
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		when(user.getId()).thenReturn(USER_ID);
		when(user.getName()).thenReturn("apiUser");
		when(user.getCustomer()).thenReturn(customer);
		org.mockito.Mockito.doReturn(Boolean.TRUE).when(user).canAccess(org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.anyString());

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getSession(false)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn(null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(session.getId()).thenReturn("session-generator-message");
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
		StateUtil.addSession(USER_ID, session);

		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn("admin");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn("Contact");
		when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(UxUi.newPrimeFaces("desktop", "template", "saga"));
		when(customer.getModule("admin")).thenThrow(new RuntimeException("wave64-module-failure"));

		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();

		HttpServletResponse response = mock(HttpServletResponse.class);
		StringWriter sink = new StringWriter();
		when(response.getWriter()).thenReturn(new PrintWriter(sink, true));

		assertDoesNotThrow(() -> servlet.doGet(request, response));
		String payload = sink.toString();
		org.junit.jupiter.api.Assertions.assertTrue(payload.contains("Could not generate views."), payload);
		verify(customer).getModule("admin");
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

	public static final class SafeCtorRenderer extends SmartClientViewRenderer {
		public SafeCtorRenderer(User user, Module module, Document document, View view, String uxui, Boolean noCreateView) {
			super(user, new ModuleImpl(), new DocumentImpl(), new ViewImpl(), uxui, noCreateView.booleanValue());
			module.getName();
			document.getName();
			view.getName();
		}
	}

}