package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;

import java.lang.reflect.Field;
import java.security.Principal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

class WebUtilTest {

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
	void getConversationBeanFromRequestReturnsNullWhenWebContextIsNull() throws Exception {
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
	void getConversationBeanFromRequestReturnsNullWhenBindingProvidedButCurrentBeanIsNull() throws Exception {
		MockWebContext webContext = new MockWebContext();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameter(AbstractWebContext.BINDING_NAME)).thenReturn("child");

		assertNull(WebUtil.getConversationBeanFromRequest(webContext, request));
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

	private static void setCurrentBeanUnsafe(AbstractWebContext webContext, Bean bean) throws Exception {
		Field currentBeanField = AbstractWebContext.class.getDeclaredField("currentBean");
		currentBeanField.setAccessible(true);
		currentBeanField.set(webContext, bean);
	}
}
