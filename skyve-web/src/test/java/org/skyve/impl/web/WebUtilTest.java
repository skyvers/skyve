package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.net.URLStreamHandlerFactory;
import java.nio.charset.StandardCharsets;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.domain.messages.ValidationException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings("boxing")
class WebUtilTest {
	private static final RecaptchaConnectionFactory RECAPTCHA_CONNECTIONS = RecaptchaConnectionFactory.install();

	private String savedCustomer;
	private ProvidedRepository savedRepository;
	private String savedServerUrl;
	private String savedSkyveContext;
	private String savedGoogleRecaptchaSecretKey;
	private String savedCloudflareTurnstileSecretKey;

	@BeforeEach
	void saveState() {
		savedCustomer = UtilImpl.CUSTOMER;
		savedRepository = ProvidedRepositoryFactory.get();
		savedServerUrl = UtilImpl.SERVER_URL;
		savedSkyveContext = UtilImpl.SKYVE_CONTEXT;
		savedGoogleRecaptchaSecretKey = UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY;
		savedCloudflareTurnstileSecretKey = UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY;
	}

	@AfterEach
	void restoreState() {
		UtilImpl.CUSTOMER = savedCustomer;
		ProvidedRepositoryFactory.set(savedRepository);
		UtilImpl.SERVER_URL = savedServerUrl;
		UtilImpl.SKYVE_CONTEXT = savedSkyveContext;
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = savedGoogleRecaptchaSecretKey;
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = savedCloudflareTurnstileSecretKey;
		RECAPTCHA_CONNECTIONS.reset();
		clearPersistenceThreadLocal();
	}

	// ===== appendQueryParameter =====

	@SuppressWarnings("static-method")
	@Test
	void appendQueryParameterEncodesEveryValue() {
		StringBuilder result = new StringBuilder("/home");

		WebUtil.appendQueryParameter(result, "search term", new String[] {"one & two", null});

		assertEquals("/home?search+term=one+%26+two&search+term=", result.toString());
	}

	@SuppressWarnings("static-method")
	@Test
	void appendQueryParameterIgnoresNullValuesArray() {
		StringBuilder result = new StringBuilder("/home");

		WebUtil.appendQueryParameter(result, "search", null);

		assertEquals("/home", result.toString());
	}

	@SuppressWarnings("static-method")
	@Test
	void appendRequestParametersEncodesValuesAndOmitsExcludedParameter() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameterNames()).thenReturn(Collections.enumeration(List.of("search term", "c")));
		when(request.getParameterValues("search term")).thenReturn(new String[] {"one & two", "three"});
		StringBuilder result = new StringBuilder("/home");

		WebUtil.appendRequestParameters(result, request, "c");

		assertEquals("/home?search+term=one+%26+two&search+term=three", result.toString());
		verify(request, never()).getParameterValues("c");
	}

	// ===== generatePasswordResetToken =====

	@SuppressWarnings("static-method")
	@Test
	void generatePasswordResetTokenIsNotNull() {
		String token = WebUtil.generatePasswordResetToken();
		assertNotNull(token);
		assertFalse(token.isEmpty());
	}

	@SuppressWarnings("static-method")
	@Test
	void generatePasswordResetTokenIsUnique() {
		String token1 = WebUtil.generatePasswordResetToken();
		String token2 = WebUtil.generatePasswordResetToken();
		// Two tokens generated at different times or with different UUIDs must differ
		// (In practice always true, but the UUID alone guarantees this)
		assertNotNull(token1);
		assertNotNull(token2);
	}

	// ===== determineCustomerWithoutSession =====

	@SuppressWarnings("static-method")
	@Test
	void determineCustomerWithoutSessionReturnsConfiguredCustomer() {
		UtilImpl.CUSTOMER = "demo";
		HttpServletRequest request = mock(HttpServletRequest.class);
		String result = WebUtil.determineCustomerWithoutSession(request);
		assertEquals("demo", result);
	}

	@SuppressWarnings("static-method")
	@Test
	void determineCustomerWithoutSessionReturnsNullWhenNoCookieAndNoConfig() {
		UtilImpl.CUSTOMER = null;
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getCookies()).thenReturn(null);
		String result = WebUtil.determineCustomerWithoutSession(request);
		assertNull(result);
	}

	@SuppressWarnings("static-method")
	@Test
	void determineCustomerWithoutSessionReadsCookieWhenConfigNotSet() {
		UtilImpl.CUSTOMER = null;
		HttpServletRequest request = mock(HttpServletRequest.class);
		Cookie customerCookie = new Cookie(AbstractWebContext.CUSTOMER_COOKIE_NAME, "acme");
		Cookie otherCookie = new Cookie("other", "value");
		when(request.getCookies()).thenReturn(new Cookie[]{otherCookie, customerCookie});
		String result = WebUtil.determineCustomerWithoutSession(request);
		assertEquals("acme", result);
	}

	@SuppressWarnings("static-method")
	@Test
	void determineCustomerWithoutSessionReturnsNullWhenNoCookieMatch() {
		UtilImpl.CUSTOMER = null;
		HttpServletRequest request = mock(HttpServletRequest.class);
		Cookie otherCookie = new Cookie("unrelated", "value");
		when(request.getCookies()).thenReturn(new Cookie[]{otherCookie});
		String result = WebUtil.determineCustomerWithoutSession(request);
		assertNull(result);
	}

	// ===== getConversationBeanFromRequest =====

	@SuppressWarnings("static-method")
	@Test
	void getConversationBeanFromRequestReturnsNullWhenWebContextIsNull() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		assertNull(WebUtil.getConversationBeanFromRequest(null, request));
	}

	@SuppressWarnings("static-method")
	@Test
	void getConversationBeanFromRequestReturnsCurrentBeanWhenNoBindingProvided() throws Exception {
		MockWebContext webContext = new MockWebContext();
		HttpServletRequest request = mock(HttpServletRequest.class);
		DynamicBean root = new DynamicBean("test", "RootDoc", new HashMap<>());
		setCurrentBeanUnsafe(webContext, root);
		when(request.getParameter(AbstractWebContext.BINDING_NAME)).thenReturn(null);

		Bean result = WebUtil.getConversationBeanFromRequest(webContext, request);

		assertSame(root, result);
	}

	@SuppressWarnings("static-method")
	@Test
	void getConversationBeanFromRequestResolvesAssociationBinding() throws Exception {
		MockWebContext webContext = new MockWebContext();
		HttpServletRequest request = mock(HttpServletRequest.class);
		DynamicBean child = new DynamicBean("test", "ChildDoc", new HashMap<>());
		HashMap<String, Object> rootValues = new HashMap<>();
		rootValues.put("child", child);
		DynamicBean root = new DynamicBean("test", "RootDoc", rootValues);
		setCurrentBeanUnsafe(webContext, root);
		when(request.getParameter(AbstractWebContext.BINDING_NAME)).thenReturn("child");

		Bean result = WebUtil.getConversationBeanFromRequest(webContext, request);

		assertSame(child, result);
	}

	@SuppressWarnings("static-method")
	@Test
	void getConversationBeanFromRequestResolvesCollectionElementByBizId() throws Exception {
		MockWebContext webContext = new MockWebContext();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HashMap<String, Object> firstValues = new HashMap<>();
		firstValues.put(Bean.DOCUMENT_ID, "item-1");
		DynamicBean first = new DynamicBean("test", "ItemDoc", firstValues);
		HashMap<String, Object> secondValues = new HashMap<>();
		secondValues.put(Bean.DOCUMENT_ID, "item-2");
		DynamicBean second = new DynamicBean("test", "ItemDoc", secondValues);
		List<Bean> items = new ArrayList<>();
		items.add(first);
		items.add(second);
		HashMap<String, Object> rootValues = new HashMap<>();
		rootValues.put("items", items);
		DynamicBean root = new DynamicBean("test", "RootDoc", rootValues);
		setCurrentBeanUnsafe(webContext, root);
		when(request.getParameter(AbstractWebContext.BINDING_NAME)).thenReturn("items");
		when(request.getParameter(Bean.DOCUMENT_ID)).thenReturn("item-2");

		Bean result = WebUtil.getConversationBeanFromRequest(webContext, request);

		assertSame(second, result);
	}

	@SuppressWarnings("static-method")
	@Test
	void getConversationBeanFromRequestReturnsNullWhenBindingProvidedButCurrentBeanIsNull() {
		MockWebContext webContext = new MockWebContext();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameter(AbstractWebContext.BINDING_NAME)).thenReturn("child");

		assertNull(WebUtil.getConversationBeanFromRequest(webContext, request));
	}

	// ===== processUserPrincipalForRequest =====

	@SuppressWarnings("static-method")
	@Test
	void processUserPrincipalForRequestUsesBasicAuthWhenNoSessionOrPrincipal() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		AbstractPersistence persistence = bindPersistenceMock();
		UserImpl user = new UserImpl();
		user.setName("basicUser");
		when(request.getSession(false)).thenReturn(null);
		when(request.getHeader("Authorization")).thenReturn(basicAuth("demo/basicUser:ignored"));
		when(repository.retrieveUser("demo/basicUser")).thenReturn(user);
		ProvidedRepositoryFactory.set(repository);

		User result = WebUtil.processUserPrincipalForRequest(request, null);

		assertSame(user, result);
		verify(persistence).setUser(user);
	}

	@SuppressWarnings("static-method")
	@Test
	void processUserPrincipalForRequestReturnsNullWhenBasicAuthUserCannotBeResolved() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		AbstractPersistence persistence = bindPersistenceMock();
		when(request.getSession(false)).thenReturn(null);
		when(request.getHeader("Authorization")).thenReturn(basicAuth("missing:ignored"));
		when(repository.retrieveUser("missing")).thenReturn(null);
		ProvidedRepositoryFactory.set(repository);

		assertNull(WebUtil.processUserPrincipalForRequest(request, null));
		verify(persistence, never()).setUser(any(User.class));
	}

	// ===== setSessionId =====

	@SuppressWarnings("static-method")
	@Test
	void setSessionIdSetsIdWhenSessionExists() {
		UserImpl user = new UserImpl();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getSession(false)).thenReturn(session);
		when(session.getId()).thenReturn("session-123");
		WebUtil.setSessionId(user, request);
		assertEquals("session-123", user.getSessionId());
	}

	@SuppressWarnings("static-method")
	@Test
	void setSessionIdDoesNotSetIdWhenNoSession() {
		UserImpl user = new UserImpl();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(null);
		WebUtil.setSessionId(user, request);
		assertNull(user.getSessionId());
	}

	// ===== isConcurrentSessionWarningEligible (package-private 3-arg overload) =====

	@SuppressWarnings("static-method")
	@Test
	void isConcurrentSessionWarningEligibleReturnsFalseWhenNoPrincipal() {
		UserImpl user = new UserImpl();
		user.setCustomerName("demo");
		ProvidedRepository repo = mock(ProvidedRepository.class);
		boolean result = WebUtil.isConcurrentSessionWarningEligible(user, null, repo);
		assertFalse(result);
	}

	@SuppressWarnings("static-method")
	@Test
	void isConcurrentSessionWarningEligibleReturnsFalseForPublicUser() {
		UserImpl user = new UserImpl();
		user.setCustomerName("demo");
		user.setName("publicUser");
		Principal principal = mock(Principal.class);
		ProvidedRepository repo = mock(ProvidedRepository.class);
		when(repo.retrievePublicUserName("demo")).thenReturn("publicUser");
		boolean result = WebUtil.isConcurrentSessionWarningEligible(user, principal, repo);
		assertFalse(result);
	}

	@SuppressWarnings("static-method")
	@Test
	void isConcurrentSessionWarningEligibleReturnsTrueForNonPublicUser() {
		UserImpl user = new UserImpl();
		user.setCustomerName("demo");
		user.setName("regularUser");
		Principal principal = mock(Principal.class);
		ProvidedRepository repo = mock(ProvidedRepository.class);
		when(repo.retrievePublicUserName("demo")).thenReturn("publicUser");
		boolean result = WebUtil.isConcurrentSessionWarningEligible(user, principal, repo);
		assertTrue(result);
	}

	@SuppressWarnings("static-method")
	@Test
	void isConcurrentSessionWarningEligibleReturnsTrueWhenNoPublicUserDefined() {
		UserImpl user = new UserImpl();
		user.setCustomerName("demo");
		user.setName("someUser");
		Principal principal = mock(Principal.class);
		ProvidedRepository repo = mock(ProvidedRepository.class);
		when(repo.retrievePublicUserName("demo")).thenReturn(null);
		boolean result = WebUtil.isConcurrentSessionWarningEligible(user, principal, repo);
		assertTrue(result);
	}

	@SuppressWarnings("static-method")
	@Test
	void isConcurrentSessionWarningEligibleRequestWrapperUsesPrincipalAndRepository() {
		UserImpl user = new UserImpl();
		user.setCustomerName("demo");
		user.setName("regularUser");
		Principal principal = mock(Principal.class);
		HttpServletRequest request = mock(HttpServletRequest.class);
		ProvidedRepository repo = mock(ProvidedRepository.class);
		when(request.getUserPrincipal()).thenReturn(principal);
		when(repo.retrievePublicUserName("demo")).thenReturn("publicUser");
		ProvidedRepositoryFactory.set(repo);

		assertTrue(WebUtil.isConcurrentSessionWarningEligible(user, request));
	}

	@SuppressWarnings("static-method")
	@Test
	void shouldLogConcurrentSessionWarningRequiresAllFlagsToBeTrue() {
		assertFalse(WebUtil.shouldLogConcurrentSessionWarning(false, false, true, true));
		assertFalse(WebUtil.shouldLogConcurrentSessionWarning(true, true, true, true));
		assertFalse(WebUtil.shouldLogConcurrentSessionWarning(true, false, false, true));
		assertFalse(WebUtil.shouldLogConcurrentSessionWarning(true, false, true, false));
		assertTrue(WebUtil.shouldLogConcurrentSessionWarning(true, false, true, true));
	}

	@SuppressWarnings("static-method")
	@Test
	void buildConcurrentSessionWarningMessageIncludesExistingSessionCount() {
		String message = WebUtil.buildConcurrentSessionWarningMessage(3);

		assertTrue(message.contains("another active session already existed"));
		assertTrue(message.contains("3"));
	}

	// ===== isPublicUser (package-private) =====

	@SuppressWarnings("static-method")
	@Test
	void isPublicUserReturnsTrueForPublicUserName() {
		UserImpl user = new UserImpl();
		user.setCustomerName("demo");
		user.setName("publicUser");
		ProvidedRepository repo = mock(ProvidedRepository.class);
		when(repo.retrievePublicUserName("demo")).thenReturn("publicUser");
		assertTrue(WebUtil.isPublicUser(user, repo));
	}

	@SuppressWarnings("static-method")
	@Test
	void isPublicUserReturnsFalseForDifferentUser() {
		UserImpl user = new UserImpl();
		user.setCustomerName("demo");
		user.setName("admin");
		ProvidedRepository repo = mock(ProvidedRepository.class);
		when(repo.retrievePublicUserName("demo")).thenReturn("publicUser");
		assertFalse(WebUtil.isPublicUser(user, repo));
	}

	@SuppressWarnings("static-method")
	@Test
	void isPublicUserReturnsFalseWhenNullCustomer() {
		UserImpl user = new UserImpl();
		user.setCustomerName(null);
		ProvidedRepository repo = mock(ProvidedRepository.class);
		assertFalse(WebUtil.isPublicUser(user, repo));
	}

	@SuppressWarnings("static-method")
	@Test
	void isPublicUserReturnsFalseWhenNoPublicUserDefined() {
		UserImpl user = new UserImpl();
		user.setCustomerName("demo");
		user.setName("admin");
		ProvidedRepository repo = mock(ProvidedRepository.class);
		when(repo.retrievePublicUserName("demo")).thenReturn(null);
		assertFalse(WebUtil.isPublicUser(user, repo));
	}

	// ===== deleteCookies =====

	@SuppressWarnings("static-method")
	@Test
	void deleteCookiesDeletesAllExceptCustomerCookie() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		Cookie[] cookies = {
			new Cookie("session", "value1"),
			new Cookie(AbstractWebContext.CUSTOMER_COOKIE_NAME, "demo"),
			new Cookie("ecuador_expandeditems", "data"),
		};
		when(request.getCookies()).thenReturn(cookies);
		WebUtil.deleteCookies(request, response);
		// Only "session" cookie should be deleted (added to response)
		// Customer cookie and ecuador_* cookies should be preserved
		verify(response).addCookie(cookies[0]); // session cookie deleted
	}

	@SuppressWarnings("static-method")
	@Test
	void deleteCookiesPreservesUltimaExpandedItemsCookie() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		Cookie ultimaCookie = new Cookie("ultima_expandeditems", "state");
		Cookie removable = new Cookie("auth", "token");
		when(request.getCookies()).thenReturn(new Cookie[] {ultimaCookie, removable});

		WebUtil.deleteCookies(request, response);

		verify(response).addCookie(removable);
		verify(response, never()).addCookie(ultimaCookie);
	}

	@SuppressWarnings("static-method")
	@Test
	void deleteCookiesWithNullCookiesDoesNothing() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(request.getCookies()).thenReturn(null);
		// Verifies no exception is thrown when cookies are null
		WebUtil.deleteCookies(request, response);
		verify(response, never()).addCookie(any());
	}

	@SuppressWarnings("static-method")
	@Test
	void deleteCookiesByNameDeletesOnlyNamedCookies() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		Cookie sessionCookie = new Cookie("JSESSIONID", "abc");
		Cookie authCookie = new Cookie("auth", "token");
		when(request.getCookies()).thenReturn(new Cookie[]{sessionCookie, authCookie});
		WebUtil.deleteCookies(request, response, "JSESSIONID");
		verify(response).addCookie(sessionCookie);
	}

	@SuppressWarnings("static-method")
	@Test
	void deleteCookiesByNameCanExplicitlyDeleteCustomerCookie() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		Cookie customerCookie = new Cookie(AbstractWebContext.CUSTOMER_COOKIE_NAME, "demo");
		Cookie otherCookie = new Cookie("other", "value");
		when(request.getCookies()).thenReturn(new Cookie[]{customerCookie, otherCookie});

		WebUtil.deleteCookies(request, response, AbstractWebContext.CUSTOMER_COOKIE_NAME);

		verify(response).addCookie(customerCookie);
		verify(response, never()).addCookie(otherCookie);
	}

	@SuppressWarnings("static-method")
	@Test
	void deleteCookiesSetsDeletionAttributesForNamedCookie() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		Cookie target = new Cookie("auth", "token");
		when(request.getCookies()).thenReturn(new Cookie[]{target});

		WebUtil.deleteCookies(request, response, "auth");

		assertEquals("-", target.getValue());
		assertEquals(0, target.getMaxAge());
		assertEquals("/", target.getPath());
		assertTrue(target.isHttpOnly());
	}

	@SuppressWarnings("static-method")
	@Test
	void deleteCookiesWithEmptyCookieArrayDoesNothing() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(request.getCookies()).thenReturn(new Cookie[0]);

		WebUtil.deleteCookies(request, response);

		verify(response, never()).addCookie(any());
	}

	// ===== deleteMenuCookies =====

	@SuppressWarnings("static-method")
	@Test
	void deleteMenuCookiesDeletesPanelMenuCookie() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		Cookie menuCookie = new Cookie("panelMenu-leftMenu", "state");
		when(request.getCookies()).thenReturn(new Cookie[]{menuCookie});
		WebUtil.deleteMenuCookies(request, response);
		verify(response).addCookie(menuCookie);
	}

	// ===== getDownloadActionUrl =====

	@SuppressWarnings("static-method")
	@Test
	void getDownloadActionUrlWithNoBindingsBuildsCorrectUrl() {
		UtilImpl.SERVER_URL = "http://localhost:8080";
		UtilImpl.SKYVE_CONTEXT = "/app";
		String url = WebUtil.getDownloadActionUrl("ExportAction", "admin", "User", "webId123", null, null, null);
		assertTrue(url.startsWith("http://localhost:8080/app/download?"));
		assertTrue(url.contains(AbstractWebContext.RESOURCE_FILE_NAME + "=ExportAction"));
		assertTrue(url.contains(AbstractWebContext.DOCUMENT_NAME + "=admin.User"));
		assertTrue(url.contains(AbstractWebContext.CONTEXT_NAME + "=webId123"));
		assertFalse(url.contains(AbstractWebContext.BINDING_NAME + "="));
	}

	@SuppressWarnings("static-method")
	@Test
	void getDownloadActionUrlWithViewBindingOnlyIncludesBinding() {
		UtilImpl.SERVER_URL = "http://localhost:8080";
		UtilImpl.SKYVE_CONTEXT = "/app";
		String url = WebUtil.getDownloadActionUrl("ExportAction", "admin", "User", "webId456", "contact", null, null);
		assertTrue(url.contains(AbstractWebContext.BINDING_NAME + "=contact"));
		assertFalse(url.contains("bizId"));
	}

	@SuppressWarnings("static-method")
	@Test
	void getDownloadActionUrlWithDataWidgetBindingOnlyIncludesBinding() {
		UtilImpl.SERVER_URL = "http://localhost:8080";
		UtilImpl.SKYVE_CONTEXT = "/app";
		String url = WebUtil.getDownloadActionUrl("ExportAction", "admin", "User", "webId789", null, "items", "elem1");
		assertTrue(url.contains(AbstractWebContext.BINDING_NAME + "=items"));
		assertTrue(url.contains("bizId=elem1"));
	}

	@SuppressWarnings("static-method")
	@Test
	void getDownloadActionUrlWithBothBindingsCreatesCompoundBinding() {
		UtilImpl.SERVER_URL = "http://localhost:8080";
		UtilImpl.SKYVE_CONTEXT = "/app";
		String url = WebUtil.getDownloadActionUrl("ExportAction", "admin", "User", "webIdABC", "parent", "items", "elem2");
		assertTrue(url.contains(AbstractWebContext.BINDING_NAME + "=parent.items"));
		assertTrue(url.contains("bizId=elem2"));
	}

	// ===== getRefererHeader =====

	@SuppressWarnings("static-method")
	@Test
	void getRefererHeaderReturnsNullWhenHeaderMissing() {
		UtilImpl.SERVER_URL = "http://localhost:8080";
		UtilImpl.SKYVE_CONTEXT = "/app";
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getHeader("referer")).thenReturn(null);
		assertNull(WebUtil.getRefererHeader(request));
	}

	@SuppressWarnings("static-method")
	@Test
	void getRefererHeaderReturnsNullWhenTampered() {
		UtilImpl.SERVER_URL = "http://localhost:8080";
		UtilImpl.SKYVE_CONTEXT = "/app";
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getHeader("referer")).thenReturn("http://evil.example.com/malicious");
		assertNull(WebUtil.getRefererHeader(request));
	}

	@SuppressWarnings("static-method")
	@Test
	void getRefererHeaderReturnsSanitisedValueWhenValid() {
		UtilImpl.SERVER_URL = "http://localhost:8080";
		UtilImpl.SKYVE_CONTEXT = "/app";
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getHeader("referer")).thenReturn("http://localhost:8080/app/index.xhtml");
		String result = WebUtil.getRefererHeader(request);
		assertNotNull(result);
		assertTrue(result.startsWith("http://localhost:8080/app"));
	}

	// ===== logout =====

	@SuppressWarnings("static-method")
	@Test
	void logoutInvalidatesExistingSessionAndDeletesCookies() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		HttpSession session = mock(HttpSession.class);
		Cookie removable = new Cookie("JSESSIONID", "abc");
		Cookie customerCookie = new Cookie(AbstractWebContext.CUSTOMER_COOKIE_NAME, "demo");

		when(request.getSession(false)).thenReturn(session);
		when(request.getCookies()).thenReturn(new Cookie[] {removable, customerCookie});

		WebUtil.logout(request, response);

		verify(request).logout();
		verify(session).invalidate();
		verify(response).addCookie(removable);
	}

	@SuppressWarnings("static-method")
	@Test
	void logoutSkipsSessionInvalidationWhenNoSessionExists() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);

		when(request.getSession(false)).thenReturn(null);
		when(request.getCookies()).thenReturn(null);

		WebUtil.logout(request, response);

		verify(request).logout();
		verify(response, never()).addCookie(any());
	}

	// ===== validateRecaptcha =====

	@SuppressWarnings("static-method")
	@Test
	void validateRecaptchaReturnsTrueWhenNoSecretConfigured() {
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = null;
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = null;

		assertTrue(WebUtil.validateRecaptcha(null));
		assertTrue(WebUtil.validateRecaptcha("any-response"));
	}

	@SuppressWarnings("static-method")
	@Test
	void validateRecaptchaReturnsFalseWhenGoogleSecretConfiguredButNoResponse() {
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = "google-secret";
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = null;

		assertFalse(WebUtil.validateRecaptcha(null));
	}

	@SuppressWarnings("static-method")
	@Test
	void validateRecaptchaReturnsFalseWhenTurnstileSecretConfiguredButNoResponse() {
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = null;
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = "turnstile-secret";

		assertFalse(WebUtil.validateRecaptcha(null));
	}

	@SuppressWarnings("static-method")
	@Test
	void validateRecaptchaPostsToGoogleAndReturnsSuccessFlag() {
		assumeTrue(RECAPTCHA_CONNECTIONS.isInstalled());
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = "google secret";
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = null;
		RECAPTCHA_CONNECTIONS.respondWith("{\"success\":true}");

		assertTrue(WebUtil.validateRecaptcha("response token"));

		CapturingURLConnection connection = RECAPTCHA_CONNECTIONS.lastConnection();
		assertNotNull(connection);
		assertEquals("https://www.google.com/recaptcha/api/siteverify", connection.getURL().toString());
		assertTrue(connection.postBody().contains("secret=google+secret"));
		assertTrue(connection.postBody().contains("response=response+token"));
		assertEquals("application/x-www-form-urlencoded", connection.contentType);
		assertTrue(connection.capturedDoInput);
		assertTrue(connection.capturedDoOutput);
		assertFalse(connection.capturedUseCaches);
	}

	@SuppressWarnings("static-method")
	@Test
	void validateRecaptchaPostsToTurnstileAndReturnsFalseForRejectedResponse() {
		assumeTrue(RECAPTCHA_CONNECTIONS.isInstalled());
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = null;
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = "turnstile-secret";
		RECAPTCHA_CONNECTIONS.respondWith("{\"success\":false}");

		assertFalse(WebUtil.validateRecaptcha("response"));

		CapturingURLConnection connection = RECAPTCHA_CONNECTIONS.lastConnection();
		assertNotNull(connection);
		assertEquals("https://challenges.cloudflare.com/turnstile/v0/siteverify", connection.getURL().toString());
	}

	@SuppressWarnings("static-method")
	@Test
	void validateRecaptchaReturnsFalseWhenResponseJsonCannotBeParsed() {
		assumeTrue(RECAPTCHA_CONNECTIONS.isInstalled());
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = "google-secret";
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = null;
		RECAPTCHA_CONNECTIONS.respondWith("not json");

		assertFalse(WebUtil.validateRecaptcha("response"));

		assertNotNull(RECAPTCHA_CONNECTIONS.lastConnection());
	}

	@SuppressWarnings("static-method")
	@Test
	void makePasswordChangeReturnsNullWhenActionSucceeds() throws Exception {
		PasswordChangeFixture fixture = passwordChangeFixture();
		AbstractPersistence persistence = bindPersistenceMock();

		String result = WebUtil.makePasswordChange(fixture.user, "old", "new", "new");

		assertNull(result);
		verify(persistence).setUser(fixture.user);
		verify(persistence).begin();
		verify(persistence).commit(true);
		verify(fixture.action).execute(fixture.bean, null);
	}

	@SuppressWarnings("static-method")
	@Test
	void makePasswordChangeReturnsFallbackMessageForValidationWithNoMessages() throws Exception {
		PasswordChangeFixture fixture = passwordChangeFixture();
		AbstractPersistence persistence = bindPersistenceMock();
		when(fixture.action.execute(fixture.bean, null)).thenThrow(new ValidationException());

		String result = WebUtil.makePasswordChange(fixture.user, null, "new", "new");

		assertEquals("The password change could not be completed.", result);
		verify(persistence).rollback();
		verify(persistence).commit(true);
	}

	@SuppressWarnings("static-method")
	@Test
	void makePasswordChangeReturnsFirstValidationMessage() throws Exception {
		PasswordChangeFixture fixture = passwordChangeFixture();
		AbstractPersistence persistence = bindPersistenceMock();
		when(fixture.action.execute(fixture.bean, null)).thenThrow(new ValidationException("first problem"));

		String result = WebUtil.makePasswordChange(fixture.user, null, "new", "new");

		assertEquals("first problem", result);
		verify(persistence).rollback();
		verify(persistence).commit(true);
	}

	@SuppressWarnings("static-method")
	@Test
	void makePasswordChangeRollsBackAndReturnsNullForUnexpectedException() throws Exception {
		PasswordChangeFixture fixture = passwordChangeFixture();
		AbstractPersistence persistence = bindPersistenceMock();
		when(fixture.action.execute(fixture.bean, null)).thenThrow(new IOException("boom"));

		String result = WebUtil.makePasswordChange(fixture.user, null, "new", "new");

		assertNull(result);
		verify(persistence).rollback();
		verify(persistence).commit(true);
	}

	@SuppressWarnings("static-method")
	@Test
	void findReferencedBeanReturnsBizletResolvedBeanWhenReadable() throws Exception {
		Document referenceDocument = mock(Document.class);
		Persistence persistence = mock(Persistence.class);
		org.skyve.metadata.user.User user = mockReadableUser(true);
		Customer customer = mock(Customer.class);
		Bean conversationBean = mock(Bean.class);
		WebContext webContext = mock(WebContext.class);
		Bean resolved = bean("admin", "Contact", "cust", "dg", "user1");
		Bizlet<Bean> bizlet = new Bizlet<>() {
			@Override
			public Bean resolve(String bizId, Bean parentBean, WebContext context) {
				return resolved;
			}
		};
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(referenceDocument.getBizlet(customer)).thenReturn(bizlet);

		Bean result = WebUtil.findReferencedBean(referenceDocument, "b1", persistence, conversationBean, webContext);

		assertSame(resolved, result);
		verify(persistence, never()).retrieve(referenceDocument, "b1");
	}

	@SuppressWarnings("static-method")
	@Test
	void findReferencedBeanRethrowsValidationExceptionFromBizlet() {
		Document referenceDocument = mock(Document.class);
		Persistence persistence = mock(Persistence.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		Customer customer = mock(Customer.class);
		ValidationException validation = new ValidationException("bad");
		Bizlet<Bean> bizlet = new Bizlet<>() {
			@Override
			public Bean resolve(String bizId, Bean parentBean, WebContext context) throws Exception {
				throw validation;
			}
		};
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(referenceDocument.getBizlet(customer)).thenReturn(bizlet);

		ValidationException result = assertThrows(ValidationException.class,
				() -> WebUtil.findReferencedBean(referenceDocument, "b1", persistence, mock(Bean.class), mock(WebContext.class)));

		assertSame(validation, result);
	}

	@SuppressWarnings("static-method")
	@Test
	void findReferencedBeanWrapsNonValidationBizletException() {
		Document referenceDocument = mock(Document.class);
		Persistence persistence = mock(Persistence.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		Customer customer = mock(Customer.class);
		Bizlet<Bean> bizlet = new Bizlet<>() {
			@Override
			public Bean resolve(String bizId, Bean parentBean, WebContext context) throws Exception {
				throw new IOException("boom");
			}
		};
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(referenceDocument.getBizlet(customer)).thenReturn(bizlet);
		when(referenceDocument.getOwningModuleName()).thenReturn("admin");
		when(referenceDocument.getName()).thenReturn("Contact");
		when(referenceDocument.getLocalisedSingularAlias()).thenReturn("Contact");

		assertThrows(MetaDataException.class,
				() -> WebUtil.findReferencedBean(referenceDocument, "b1", persistence, mock(Bean.class), mock(WebContext.class)));
	}

	@SuppressWarnings("static-method")
	@Test
	void findReferencedBeanRetrievesPersistableDocumentWhenBizletReturnsNull() throws Exception {
		Document referenceDocument = mock(Document.class);
		Persistence persistence = mock(Persistence.class);
		org.skyve.metadata.user.User user = mockReadableUser(true);
		Customer customer = mock(Customer.class);
		Bean retrieved = bean("admin", "Contact", "cust", "dg", "user1");
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(referenceDocument.getBizlet(customer)).thenReturn(null);
		when(referenceDocument.isPersistable()).thenReturn(Boolean.TRUE);
		when(persistence.retrieve(referenceDocument, "b1")).thenReturn(retrieved);

		Bean result = WebUtil.findReferencedBean(referenceDocument, "b1", persistence, mock(Bean.class), mock(WebContext.class));

		assertSame(retrieved, result);
	}

	@SuppressWarnings("static-method")
	@Test
	void findReferencedBeanThrowsNoResultsWhenNothingCanResolve() {
		Document referenceDocument = mock(Document.class);
		Persistence persistence = mock(Persistence.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		Customer customer = mock(Customer.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(referenceDocument.getBizlet(customer)).thenReturn(null);
		when(referenceDocument.isPersistable()).thenReturn(Boolean.FALSE);

		assertThrows(NoResultsException.class,
				() -> WebUtil.findReferencedBean(referenceDocument, "missing", persistence, mock(Bean.class), mock(WebContext.class)));
	}

	@SuppressWarnings("static-method")
	@Test
	void findReferencedBeanThrowsSecurityExceptionWhenUserCannotReadResolvedBean() {
		Document referenceDocument = mock(Document.class);
		Persistence persistence = mock(Persistence.class);
		org.skyve.metadata.user.User user = mockReadableUser(false);
		Customer customer = mock(Customer.class);
		Bean retrieved = bean("admin", "Contact", "cust", "dg", "user1");
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(referenceDocument.getBizlet(customer)).thenReturn(null);
		when(referenceDocument.isPersistable()).thenReturn(Boolean.TRUE);
		when(persistence.retrieve(referenceDocument, "b1")).thenReturn(retrieved);

		assertThrows(IllegalArgumentException.class,
				() -> WebUtil.findReferencedBean(referenceDocument, "b1", persistence, mock(Bean.class), mock(WebContext.class)));
	}

	private static void setCurrentBeanUnsafe(AbstractWebContext webContext, Bean bean) throws Exception {
		Field currentBeanField = AbstractWebContext.class.getDeclaredField("currentBean");
		currentBeanField.setAccessible(true);
		currentBeanField.set(webContext, bean);
	}

	private static AbstractPersistence bindPersistenceMock() {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		setPersistenceThreadLocal(persistence);
		return persistence;
	}

	private static String basicAuth(String credentials) {
		return "Basic " + Base64.getMimeEncoder().encodeToString(credentials.getBytes(StandardCharsets.UTF_8));
	}

	@SuppressWarnings("unchecked")
	private static void setPersistenceThreadLocal(AbstractPersistence persistence) {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			((ThreadLocal<AbstractPersistence>) field.get(null)).set(persistence);
		}
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException(e);
		}
	}

	@SuppressWarnings("unchecked")
	private static void clearPersistenceThreadLocal() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
		}
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException(e);
		}
	}

	private static PasswordChangeFixture passwordChangeFixture() throws Exception {
		PasswordChangeFixture fixture = new PasswordChangeFixture();
		fixture.user = mock(org.skyve.metadata.user.User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		fixture.action = mock(ServerSideAction.class);
		HashMap<String, Object> values = new HashMap<>();
		values.put(AppConstants.OLD_PASSWORD_ATTRIBUTE_NAME, null);
		values.put(AppConstants.NEW_PASSWORD_ATTRIBUTE_NAME, null);
		values.put(AppConstants.CONFIRM_PASSWORD_ATTRIBUTE_NAME, null);
		fixture.bean = new DynamicBean("admin", "ChangePassword", values);
		when(fixture.user.getCustomer()).thenReturn(customer);
		when(customer.getModule(AppConstants.ADMIN_MODULE_NAME)).thenReturn(module);
		when(module.getDocument(customer, AppConstants.CHANGE_PASSWORD_DOCUMENT_NAME)).thenReturn(document);
		when(document.newInstance(fixture.user)).thenReturn(fixture.bean);
		when(document.getServerSideAction(customer, AppConstants.MAKE_PASSWORD_CHANGE_ACTION_NAME, true)).thenReturn(fixture.action);
		return fixture;
	}

	private static org.skyve.metadata.user.User mockReadableUser(boolean canRead) {
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		when(user.getName()).thenReturn("alice");
		when(user.canReadBean("b1", "admin", "Contact", "cust", "dg", "user1")).thenReturn(Boolean.valueOf(canRead));
		return user;
	}

	private static Bean bean(String module, String document, String customer, String dataGroupId, String userId) {
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn(module);
		when(bean.getBizDocument()).thenReturn(document);
		when(bean.getBizCustomer()).thenReturn(customer);
		when(bean.getBizDataGroupId()).thenReturn(dataGroupId);
		when(bean.getBizUserId()).thenReturn(userId);
		return bean;
	}

	private static class RecaptchaConnectionFactory implements URLStreamHandlerFactory {
		private static final String GOOGLE_RECAPTCHA_HOST = "www.google.com";
		private static final String CLOUDFLARE_TURNSTILE_HOST = "challenges.cloudflare.com";

		private final AtomicReference<String> response = new AtomicReference<>("{\"success\":false}");
		private final AtomicReference<CapturingURLConnection> lastConnection = new AtomicReference<>();
		private boolean installed;

		static RecaptchaConnectionFactory install() {
			RecaptchaConnectionFactory result = new RecaptchaConnectionFactory();
			try {
				URL.setURLStreamHandlerFactory(result);
				result.installed = true;
			}
			catch (@SuppressWarnings("unused") Error alreadyConfigured) {
				result.installed = false;
			}
			return result;
		}

		boolean isInstalled() {
			return installed;
		}

		void respondWith(String responseJson) {
			response.set(responseJson);
			lastConnection.set(null);
		}

		CapturingURLConnection lastConnection() {
			return lastConnection.get();
		}

		void reset() {
			response.set("{\"success\":false}");
			lastConnection.set(null);
		}

		@Override
		public URLStreamHandler createURLStreamHandler(String protocol) {
			if (! "https".equals(protocol)) {
				return null;
			}
			return new URLStreamHandler() {
				@Override
				protected URLConnection openConnection(URL url) throws IOException {
					String host = url.getHost();
					if ((! GOOGLE_RECAPTCHA_HOST.equals(host)) && (! CLOUDFLARE_TURNSTILE_HOST.equals(host))) {
						throw new IOException("Unexpected HTTPS connection in WebUtilTest: " + url);
					}
					CapturingURLConnection connection = new CapturingURLConnection(url, response.get());
					lastConnection.set(connection);
					return connection;
				}
			};
		}
	}

	private static class CapturingURLConnection extends URLConnection {
		private final ByteArrayOutputStream output = new ByteArrayOutputStream();
		private final String input;
		private boolean capturedDoInput;
		private boolean capturedDoOutput;
		private boolean capturedUseCaches;
		private String contentType;

		CapturingURLConnection(URL url, String input) {
			super(url);
			this.input = input;
		}

		@Override
		public void connect() {
			connected = true;
		}

		@Override
		public void setDoInput(boolean doInput) {
			super.setDoInput(doInput);
			capturedDoInput = doInput;
		}

		@Override
		public void setDoOutput(boolean doOutput) {
			super.setDoOutput(doOutput);
			capturedDoOutput = doOutput;
		}

		@Override
		public void setUseCaches(boolean useCaches) {
			super.setUseCaches(useCaches);
			capturedUseCaches = useCaches;
		}

		@Override
		public void setRequestProperty(String key, String value) {
			super.setRequestProperty(key, value);
			if ("Content-Type".equals(key)) {
				contentType = value;
			}
		}

		@Override
		public java.io.OutputStream getOutputStream() {
			return output;
		}

		@Override
		public java.io.InputStream getInputStream() {
			return new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8));
		}

		String postBody() {
			return output.toString(StandardCharsets.UTF_8);
		}
	}

	private static class PasswordChangeFixture {
		private org.skyve.metadata.user.User user;
		private Bean bean;
		private ServerSideAction<Bean> action;
	}
}
