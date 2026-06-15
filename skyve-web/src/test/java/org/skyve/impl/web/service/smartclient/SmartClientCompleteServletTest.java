package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.RETURNS_SELF;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.security.Principal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Locale;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.web.WebContext;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings({"static-method", "resource", "java:S5976", "boxing"})
class SmartClientCompleteServletTest {
	private static final String USER_ID = "apiUserId";
	private static final String SESSION_ID = "session-1";
	private static final String CONVERSATION_KEY = "01234567-89ab-cdef-0123-456789abcdef";
	private static final String BEAN_ID = "bean-1";

	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
	private boolean originalForceNonPersistentCaching;
	private String originalCacheDirectory;
	private SessionCacheConfig originalSessionCache;
	private ConversationCacheConfig originalConversationCache;
	private Path cacheDirectory;

	@BeforeEach
	void setUp() throws Exception {
		originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		originalSessionCache = UtilImpl.SESSION_CACHE;
		originalConversationCache = UtilImpl.CONVERSATION_CACHE;

		cacheDirectory = Files.createTempDirectory("complete-servlet-cache");
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
		UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
		UtilImpl.SESSION_CACHE = new SessionCacheConfig(100, 10);
		UtilImpl.CONVERSATION_CACHE = new ConversationCacheConfig(100, 10);

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
		UtilImpl.CONVERSATION_CACHE = originalConversationCache;
		if (cacheDirectory != null) {
			try (Stream<Path> paths = Files.walk(cacheDirectory)) {
				paths.sorted((a, b) -> b.compareTo(a))
						.map(Path::toFile)
						.forEach(File::delete);
			}
		}
		ProvidedRepositoryFactory.set(originalRepository);
		clearThreadPersistence();
	}

	@Test
	void doGetWarnsWhenAttributeIsMissing() throws Exception {
		SmartClientCompleteServlet servlet = new SmartClientCompleteServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter("_attr")).thenReturn(null);
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("ctx-1");
		when(request.getParameter(AbstractWebContext.ACTION_NAME)).thenReturn("previous");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetWarnsWhenConversationIdIsMissing() throws Exception {
		SmartClientCompleteServlet servlet = new SmartClientCompleteServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter("_attr")).thenReturn("name");
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn(null);
		when(request.getParameter(AbstractWebContext.ACTION_NAME)).thenReturn("previous");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doPostWarnsWhenAttributeIsMissing() throws Exception {
		SmartClientCompleteServlet servlet = new SmartClientCompleteServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter("_attr")).thenReturn(null);
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("ctx-1");
		when(request.getParameter(AbstractWebContext.ACTION_NAME)).thenReturn("previous");

		assertDoesNotThrow(() -> servlet.doPost(request, response));
	}

	@Test
	void doGetWarnsWhenUserCannotBeResolved() throws Exception {
		SmartClientCompleteServlet servlet = new SmartClientCompleteServlet();
		HttpServletRequest request = newRequestWithoutUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter("_attr")).thenReturn("name");
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("ctx-1");
		when(request.getParameter(AbstractWebContext.ACTION_NAME)).thenReturn("previous");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetWarnsWhenActionIsInvalid() throws Exception {
		SmartClientCompleteServlet servlet = new SmartClientCompleteServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter("_attr")).thenReturn("name");
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("ctx-1");
		when(request.getParameter(AbstractWebContext.ACTION_NAME)).thenReturn("notACompleteType");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetWarnsWhenConversationCannotBeResolved() throws Exception {
		SmartClientCompleteServlet servlet = new SmartClientCompleteServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter("_attr")).thenReturn("name");
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("ctx-missing");
		when(request.getParameter(AbstractWebContext.ACTION_NAME)).thenReturn("previous");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetSuggestCompleteWritesEmptySuccessfulResponse() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		ProvidedRepositoryFactory.set(repository);

		UserImpl user = mock(UserImpl.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		DocumentImpl document = persistentDocument();
		Bean bean = conversationBean();

		when(user.getId()).thenReturn(USER_ID);
		when(user.getName()).thenReturn("apiUser");
		when(user.getCustomer()).thenReturn(customer);
		when(user.canReadDocument(document)).thenReturn(true);
		when(customer.getModule("test")).thenReturn(module);
		when(module.getDocument(customer, "Thing")).thenReturn(document);

		HttpServletRequest request = requestWithCachedConversation(user, bean);
		when(request.getParameter("_attr")).thenReturn("name");
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn(CONVERSATION_KEY + BEAN_ID);
		when(request.getParameter(AbstractWebContext.ACTION_NAME)).thenReturn("suggest");
		when(request.getParameter("value")).thenReturn("a");
		when(request.getParameter("_startRow")).thenReturn("0");
		when(request.getParameter("_endRow")).thenReturn("2");
		when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(UxUi.newPrimeFaces("desktop", "template", "saga"));

		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();

		HttpServletResponse response = mock(HttpServletResponse.class);
		StringWriter sink = new StringWriter();
		when(response.getWriter()).thenReturn(new PrintWriter(sink, true));

		assertDoesNotThrow(() -> new SmartClientCompleteServlet().doGet(request, response));

		String payload = sink.toString();
		org.junit.jupiter.api.Assertions.assertTrue(payload.contains("\"status\":0"), payload);
		org.junit.jupiter.api.Assertions.assertTrue(payload.contains("\"totalRows\":0"), payload);
		org.junit.jupiter.api.Assertions.assertTrue(payload.contains("\"data\":[]"), payload);
		verify(customer).interceptBeforeComplete("name", "a", bean);
		verify(customer).interceptAfterComplete("name", "a", bean, null);
	}

	private static DocumentImpl persistentDocument() {
		Text name = new Text();
		name.setName("name");
		name.setPersistent(Boolean.TRUE);
		name.setLength(100);

		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("test");
		document.setName("Thing");
		document.setPersistent(new Persistent());
		document.putAttribute(name);
		return document;
	}

	private static Bean conversationBean() {
		HashMap<String, Object> properties = new HashMap<>();
		properties.put(Bean.DOCUMENT_ID, BEAN_ID);
		return new DynamicBean("test", "Thing", properties);
	}

	private static HttpServletRequest requestWithCachedConversation(UserImpl user, Bean bean) throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getSession(false)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn(null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(session.getId()).thenReturn(SESSION_ID);
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
		StateUtil.addSession(USER_ID, session);

		SmartClientWebContext webContext = new SmartClientWebContext(CONVERSATION_KEY, request);
		webContext.setCurrentBean(bean);
		StateUtil.cacheConversation(webContext);
		return request;
	}

	private static HttpServletRequest newRequestWithSessionUser() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		UserImpl user = new UserImpl();
		user.setId(USER_ID);
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
		when(session.getId()).thenReturn(SESSION_ID);
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
}
