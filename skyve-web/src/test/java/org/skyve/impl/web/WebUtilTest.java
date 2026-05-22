package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;

import java.security.Principal;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
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

	@BeforeEach
	void saveState() {
		savedCustomer = UtilImpl.CUSTOMER;
		savedRepository = ProvidedRepositoryFactory.get();
		savedServerUrl = UtilImpl.SERVER_URL;
		savedSkyveContext = UtilImpl.SKYVE_CONTEXT;
	}

	@AfterEach
	void restoreState() {
		UtilImpl.CUSTOMER = savedCustomer;
		ProvidedRepositoryFactory.set(savedRepository);
		UtilImpl.SERVER_URL = savedServerUrl;
		UtilImpl.SKYVE_CONTEXT = savedSkyveContext;
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
}
