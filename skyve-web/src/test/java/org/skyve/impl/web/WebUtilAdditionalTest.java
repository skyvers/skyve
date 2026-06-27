package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * Additional unit tests for {@link WebUtil} targeting uncovered methods.
 */
@SuppressWarnings("static-method")
class WebUtilAdditionalTest {

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

	// ===== logout =====

	@Test
	void logoutLogsOutAndInvalidatesSession() throws ServletException {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getSession(false)).thenReturn(session);
		when(request.getCookies()).thenReturn(null);

		WebUtil.logout(request, response);

		verify(request).logout();
		verify(session).invalidate();
	}

	@Test
	void logoutHandlesNullSession() throws ServletException {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(request.getSession(false)).thenReturn(null);
		when(request.getCookies()).thenReturn(null);

		WebUtil.logout(request, response);

		verify(request).logout();
		verify(response, never()).addCookie(any());
	}

	// ===== getRefererHeader =====

	@Test
	void getRefererHeaderReturnsSanitizedValueForValidOrigin() {
		UtilImpl.SERVER_URL = "http://localhost:8080";
		UtilImpl.SKYVE_CONTEXT = "/app";
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getHeader("referer")).thenReturn("http://localhost:8080/app/some/page?param=value");

		String result = WebUtil.getRefererHeader(request);
		assertNotNull(result);
		assertTrue(result.startsWith("http://localhost:8080/app/some/page"));
	}

	@Test
	void getRefererHeaderReturnsSanitizedValueForExactContextUrl() {
		UtilImpl.SERVER_URL = "http://localhost:8080";
		UtilImpl.SKYVE_CONTEXT = "/app";
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getHeader("referer")).thenReturn("http://localhost:8080/app/");

		String result = WebUtil.getRefererHeader(request);
		assertNotNull(result);
		assertTrue(result.startsWith("http://localhost:8080/app/"));
	}

	// ===== deleteCookies edge cases =====

	@Test
	void deleteCookiesPreservesUltimaCookies() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		Cookie ecuadorCookie = new Cookie("ecuador_expandeditems", "data");
		Cookie ultimaCookie = new Cookie("ultima_expandeditems", "data");
		Cookie regularCookie = new Cookie("session", "val");
		when(request.getCookies()).thenReturn(new Cookie[] { ecuadorCookie, ultimaCookie, regularCookie });

		WebUtil.deleteCookies(request, response);

		verify(response).addCookie(regularCookie);
		verify(response, never()).addCookie(ecuadorCookie);
		verify(response, never()).addCookie(ultimaCookie);
	}

	@Test
	void deleteCookiesWithEmptyCookiesDoesNothing() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(request.getCookies()).thenReturn(new Cookie[] {});

		WebUtil.deleteCookies(request, response);

		verify(response, never()).addCookie(any());
	}

	// ===== getConversationBeanFromRequest edge cases =====

	@Test
	void getConversationBeanFromRequestReturnsNullWhenCurrentBeanNull() {
		MockWebContext webContext = new MockWebContext();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameter(AbstractWebContext.BINDING_NAME)).thenReturn("child");

		org.skyve.domain.Bean result = WebUtil.getConversationBeanFromRequest(webContext, request);
		assertNull(result);
	}

	// ===== validateRecaptcha =====

	@Test
	void validateRecaptchaReturnsTrueWhenNoSecretConfigured() {
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = null;
		UtilImpl.CLOUDFLARE_TURNSTILE_SECRET_KEY = null;
		assertTrue(WebUtil.validateRecaptcha(null));
	}

	@Test
	void validateRecaptchaReturnsFalseWhenNullResponse() {
		UtilImpl.GOOGLE_RECAPTCHA_SECRET_KEY = "test-secret";
		assertTrue(!WebUtil.validateRecaptcha(null));
	}

	// ===== setSessionId edge case =====

	@Test
	void setSessionIdWithNullSessionDoesNotSetId() {
		UserImpl user = new UserImpl();
		user.setSessionId("existing-id");
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(null);

		WebUtil.setSessionId(user, request);

		assertEquals("existing-id", user.getSessionId());
	}

	// ===== processUserPrincipalForRequest =====

	@Test
	void processUserPrincipalForRequestReturnsNullWhenNoSessionAndNoPrincipal() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(null);

		org.skyve.metadata.user.User user = WebUtil.processUserPrincipalForRequest(request, null);
		assertNull(user);
	}

	}