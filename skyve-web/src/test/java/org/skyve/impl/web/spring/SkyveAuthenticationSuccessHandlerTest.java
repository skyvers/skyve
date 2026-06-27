package org.skyve.impl.web.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.TwoFactorAuthConfigurationSingleton;
import org.skyve.impl.util.TwoFactorAuthCustomerConfiguration;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;
import org.springframework.security.core.Authentication;
import org.springframework.security.provisioning.UserDetailsManager;
import org.springframework.security.web.RedirectStrategy;
import org.springframework.security.web.savedrequest.DefaultSavedRequest;
import org.springframework.security.web.savedrequest.SavedRequest;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings({"static-method", "unchecked"})
class SkyveAuthenticationSuccessHandlerTest {
	private static final String SAVED_REQUEST_KEY = "SPRING_SECURITY_SAVED_REQUEST";

	private final String originalServerUrl = UtilImpl.SERVER_URL;
	private final String originalSkyveContext = UtilImpl.SKYVE_CONTEXT;
	private final String originalHomeUri = UtilImpl.HOME_URI;

	@AfterEach
	void resetEnvironment() throws Exception {
		UtilImpl.SERVER_URL = originalServerUrl;
		UtilImpl.SKYVE_CONTEXT = originalSkyveContext;
		UtilImpl.HOME_URI = originalHomeUri;
		resetSecureUrlCache();
		clearTfaConfig("acme");
		clearTfaConfig("beta");
	}

	@Test
	void redirectsHomeWhenNoSavedRequest() throws Exception {
		setUrlEnvironment("http://localhost:8080", "/skyve", "/home");
		SkyveAuthenticationSuccessHandler handler = new SkyveAuthenticationSuccessHandler(null);
		RedirectStrategy redirect = mock(RedirectStrategy.class);
		handler.setRedirectStrategy(redirect);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(null);
		HttpServletResponse response = mock(HttpServletResponse.class);
		Authentication authentication = authenticationWithPrincipal("alice");

		handler.onAuthenticationSuccess(request, response, authentication);

		verify(redirect).sendRedirect(request, response, "http://localhost:8080/skyve/home");
	}

	@Test
	void keepsRedirectForRegularXhtmlUrl() throws Exception {
		setUrlEnvironment("http://localhost:8080", "/skyve", "/home");
		SkyveAuthenticationSuccessHandler handler = new SkyveAuthenticationSuccessHandler(null);
		RedirectStrategy redirect = mock(RedirectStrategy.class);
		handler.setRedirectStrategy(redirect);

		SavedRequest savedRequest = mock(SavedRequest.class);
		when(savedRequest.getRedirectUrl()).thenReturn("http://localhost:8080/skyve/edit.xhtml?a=e");
		HttpServletRequest request = requestWithSavedRequest(savedRequest);
		HttpServletResponse response = mock(HttpServletResponse.class);

		handler.onAuthenticationSuccess(request, response, authenticationWithPrincipal("bob"));

		verify(redirect).sendRedirect(request, response, "http://localhost:8080/skyve/edit.xhtml?a=e");
	}

	@Test
	void redirectsHomeForNonPageSavedUrl() throws Exception {
		setUrlEnvironment("http://localhost:8080", "/skyve", "/home");
		SkyveAuthenticationSuccessHandler handler = new SkyveAuthenticationSuccessHandler(null);
		RedirectStrategy redirect = mock(RedirectStrategy.class);
		handler.setRedirectStrategy(redirect);

		SavedRequest savedRequest = mock(SavedRequest.class);
		when(savedRequest.getRedirectUrl()).thenReturn("http://localhost:8080/skyve/smartclient");
		HttpServletRequest request = requestWithSavedRequest(savedRequest);
		HttpServletResponse response = mock(HttpServletResponse.class);

		handler.onAuthenticationSuccess(request, response, authenticationWithPrincipal("carol"));

		verify(redirect).sendRedirect(request, response, "http://localhost:8080/skyve/home");
	}

	@Test
	void rewritesInsecureSavedRequestWhenServerIsSecure() throws Exception {
		setUrlEnvironment("https://secure.example", "/skyve", "/home");
		SkyveAuthenticationSuccessHandler handler = new SkyveAuthenticationSuccessHandler(null);
		RedirectStrategy redirect = mock(RedirectStrategy.class);
		handler.setRedirectStrategy(redirect);

		DefaultSavedRequest savedRequest = mock(DefaultSavedRequest.class);
		when(savedRequest.getRedirectUrl()).thenReturn("http://legacy.example/skyve/report.jsp");
		when(savedRequest.getRequestURI()).thenReturn("/skyve/report.jsp");
		when(savedRequest.getQueryString()).thenReturn("a=l");
		HttpServletRequest request = requestWithSavedRequest(savedRequest);
		HttpServletResponse response = mock(HttpServletResponse.class);

		handler.onAuthenticationSuccess(request, response, authenticationWithPrincipal("dave"));

		verify(redirect).sendRedirect(request, response, "https://secure.example/skyve/report.jsp?a=l");
	}

	@Test
	void fallsBackToHomeWhenSecureButSavedRequestIsNotDefaultSavedRequest() throws Exception {
		setUrlEnvironment("https://secure.example", "/skyve", "/home");
		SkyveAuthenticationSuccessHandler handler = new SkyveAuthenticationSuccessHandler(null);
		RedirectStrategy redirect = mock(RedirectStrategy.class);
		handler.setRedirectStrategy(redirect);

		SavedRequest savedRequest = mock(SavedRequest.class);
		when(savedRequest.getRedirectUrl()).thenReturn("http://legacy.example/skyve/socket");
		HttpServletRequest request = requestWithSavedRequest(savedRequest);
		HttpServletResponse response = mock(HttpServletResponse.class);

		handler.onAuthenticationSuccess(request, response, authenticationWithPrincipal("erin"));

		verify(redirect).sendRedirect(request, response, "https://secure.example/skyve/home");
	}

	@Test
	void clearsTfaCodesAndUpdatesUserWhenPushTfaEnabled() throws Exception {
		setUrlEnvironment("http://localhost:8080", "/skyve", "/home");
		putTfaConfig("acme", new TwoFactorAuthCustomerConfiguration("email", 60, "subject", "body"));
		UserDetailsManager userDetailsManager = mock(UserDetailsManager.class);
		SkyveAuthenticationSuccessHandler handler = new SkyveAuthenticationSuccessHandler(userDetailsManager);
		RedirectStrategy redirect = mock(RedirectStrategy.class);
		handler.setRedirectStrategy(redirect);

		TwoFactorAuthUser principal = new TwoFactorAuthUser("acme/alice",
				"pwd",
				true,
				true,
				true,
				true,
				Collections.emptyList(),
				"acme",
				"alice",
				"123456",
				"token",
				new org.skyve.domain.types.Timestamp(new Timestamp(System.currentTimeMillis())),
				"alice@example.com",
				"pwd");
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(null);
		HttpServletResponse response = mock(HttpServletResponse.class);

		handler.onAuthenticationSuccess(request, response, authenticationWithPrincipal(principal));

		assertNull(principal.getTfaCodeGeneratedTimestamp());
		assertNull(principal.getTfaCode());
		assertNull(principal.getTfaToken());
		verify(userDetailsManager).updateUser(principal);
	}

	@Test
	void leavesTfaCodesUnchangedWhenPushTfaDisabled() throws Exception {
		setUrlEnvironment("http://localhost:8080", "/skyve", "/home");
		putTfaConfig("beta", new TwoFactorAuthCustomerConfiguration("off", 60, "subject", "body"));
		UserDetailsManager userDetailsManager = mock(UserDetailsManager.class);
		SkyveAuthenticationSuccessHandler handler = new SkyveAuthenticationSuccessHandler(userDetailsManager);
		RedirectStrategy redirect = mock(RedirectStrategy.class);
		handler.setRedirectStrategy(redirect);

		org.skyve.domain.types.Timestamp generatedTimestamp = new org.skyve.domain.types.Timestamp(new Timestamp(System.currentTimeMillis()));
		TwoFactorAuthUser principal = new TwoFactorAuthUser("beta/bob",
				"pwd",
				true,
				true,
				true,
				true,
				Collections.emptyList(),
				"beta",
				"bob",
				"987654",
				"token-2",
				generatedTimestamp,
				"bob@example.com",
				"pwd");
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(null);
		HttpServletResponse response = mock(HttpServletResponse.class);

		handler.onAuthenticationSuccess(request, response, authenticationWithPrincipal(principal));

		assertEquals(generatedTimestamp, principal.getTfaCodeGeneratedTimestamp());
		assertEquals("987654", principal.getTfaCode());
		assertEquals("token-2", principal.getTfaToken());
		verify(userDetailsManager, never()).updateUser(any());
	}

	private static Authentication authenticationWithPrincipal(Object principal) {
		Authentication authentication = mock(Authentication.class);
		when(authentication.getPrincipal()).thenReturn(principal);
		return authentication;
	}

	private static HttpServletRequest requestWithSavedRequest(SavedRequest savedRequest) {
		HttpSession session = mock(HttpSession.class);
		when(session.getAttribute(SAVED_REQUEST_KEY)).thenReturn(savedRequest);
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(session);
		return request;
	}

	private static void setUrlEnvironment(String serverUrl, String context, String homeUri) throws Exception {
		UtilImpl.SERVER_URL = serverUrl;
		UtilImpl.SKYVE_CONTEXT = context;
		UtilImpl.HOME_URI = homeUri;
		resetSecureUrlCache();
	}

	private static void resetSecureUrlCache() throws Exception {
		Field field = Util.class.getDeclaredField("secureUrl");
		field.setAccessible(true);
		field.set(null, null);
	}

	private static void putTfaConfig(String customer, TwoFactorAuthCustomerConfiguration config) throws Exception {
		Field configurationField = TwoFactorAuthConfigurationSingleton.class.getDeclaredField("configuration");
		configurationField.setAccessible(true);
		Map<String, TwoFactorAuthCustomerConfiguration> configuration =
				(Map<String, TwoFactorAuthCustomerConfiguration>) configurationField.get(TwoFactorAuthConfigurationSingleton.getInstance());
		configuration.put(customer, config);
	}

	private static void clearTfaConfig(String customer) {
		TwoFactorAuthConfigurationSingleton.getInstance().clearConfig(customer);
	}
}
