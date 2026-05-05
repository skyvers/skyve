package org.skyve.impl.web.spring;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.util.TwoFactorAuthConfigurationSingleton;
import org.skyve.impl.util.TwoFactorAuthCustomerConfiguration;
import org.skyve.impl.util.UtilImpl;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.LockedException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.provisioning.UserDetailsManager;

import jakarta.servlet.FilterChain;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletMapping;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import jakarta.servlet.http.MappingMatch;

public class TwoFactorAuthPushFilterTest {
	private static final String CUSTOMER = "acme";
	private Set<String> previousTwoFactorCustomers;
	private Map<String, TwoFactorAuthCustomerConfiguration> previousConfigurations;
	private ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration> configurationMap;
	private UserDetailsManager userDetailsManager;
	private TestTwoFactorAuthPushFilter filter;

	@BeforeEach
	public void setUp() throws Exception {
		previousTwoFactorCustomers = UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS;
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = Set.of(CUSTOMER);

		configurationMap = getConfigurationMap();
		previousConfigurations = Map.copyOf(configurationMap);
		configurationMap.clear();

		userDetailsManager = mock(UserDetailsManager.class);
		filter = new TestTwoFactorAuthPushFilter(userDetailsManager);
	}

	@AfterEach
	public void tearDown() {
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = previousTwoFactorCustomers;
		configurationMap.clear();
		configurationMap.putAll(previousConfigurations);
	}

	@Test
	public void testSkipPushFilterWhenTwoFactorIsOff() {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("OFF", 300, "subject", "body"));
		boolean skip = filter.callSkipPushFilter(loginRequest(CUSTOMER), mock(HttpServletResponse.class));
		assertTrue(skip);
	}

	@Test
	public void testSkipPushFilterWhenTwoFactorIsEmail() {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body"));
		boolean skip = filter.callSkipPushFilter(loginRequest(CUSTOMER), mock(HttpServletResponse.class));
		assertFalse(skip);
	}

	@Test
	public void testSkipPushFilterWhenTwoFactorIsUnsupportedFactor() {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("SMS", 300, "subject", "body"));
		boolean skip = filter.callSkipPushFilter(loginRequest(CUSTOMER), mock(HttpServletResponse.class));
		assertFalse(skip);
	}

	@Test
	public void testIsPushTfaRemainsEmailOnly() {
		assertFalse(TwoFactorAuthConfigurationSingleton.isPushTfa(new TwoFactorAuthCustomerConfiguration("OFF", 300, "subject", "body")));
		assertTrue(TwoFactorAuthConfigurationSingleton.isPushTfa(new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body")));
		assertFalse(TwoFactorAuthConfigurationSingleton.isPushTfa(new TwoFactorAuthCustomerConfiguration("SMS", 300, "subject", "body")));
	}

	@Test
	public void testUnsupportedFactorDoesNotBypassMfa() throws Exception {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("SMS", 300, "subject", "body"));

		HttpServletRequest request = loginRequest(CUSTOMER);
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.isCommitted()).thenReturn(false);
		when(response.encodeRedirectURL(anyString())).thenAnswer(invocation -> invocation.getArgument(0));

		FilterChain chain = mock(FilterChain.class);
		filter.doFilter(request, response, chain);

		verify(chain, never()).doFilter(any(), any());
		verify(response).sendRedirect("/login?error");
	}

	@Test
	public void testTwoFactorCodeValidationAcceptsStrictSixDigits() {
		assertTrue(filter.callIsValidTwoFactorCode("123456"));
	}

	@Test
	public void testTwoFactorCodeValidationRejectsNonSixDigitValues() {
		assertFalse(filter.callIsValidTwoFactorCode(null));
		assertFalse(filter.callIsValidTwoFactorCode(""));
		assertFalse(filter.callIsValidTwoFactorCode("12345"));
		assertFalse(filter.callIsValidTwoFactorCode("1234567"));
		assertFalse(filter.callIsValidTwoFactorCode("12a456"));
	}

	@Test
	public void testInvalidTwoFactorFormatStillAttemptsAuthentication() throws Exception {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body"));
		HttpServletRequest request = twoFactorCodeRequest("abc");
		HttpServletResponse response = responseWithForward(request);
		FilterChain chain = mock(FilterChain.class);
		AuthenticationManager authenticationManager = mock(AuthenticationManager.class);
		filter.setAuthenticationManager(authenticationManager);
		when(authenticationManager.authenticate(any(Authentication.class))).thenThrow(new BadCredentialsException("bad code"));

		filter.doFilter(request, response, chain);

		verify(authenticationManager).authenticate(any(Authentication.class));
		verify(chain, never()).doFilter(any(), any());
	}

	@Test
	public void testBadTwoFactorCodeStaysOnTwoFactorPage() throws Exception {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body"));
		HttpServletRequest request = twoFactorCodeRequest("123456");
		HttpServletResponse response = responseWithForward(request);
		FilterChain chain = mock(FilterChain.class);
		AuthenticationManager authenticationManager = mock(AuthenticationManager.class);
		filter.setAuthenticationManager(authenticationManager);
		when(authenticationManager.authenticate(any(Authentication.class))).thenThrow(new BadCredentialsException("bad code"));

		filter.doFilter(request, response, chain);

		verify(request).setAttribute(TwoFactorAuthForwardHandler.TWO_FACTOR_AUTH_ERROR_ATTRIBUTE, "1");
		verify(chain, never()).doFilter(any(), any());
	}

	@Test
	public void testLockedTwoFactorAttemptRedirectsToLoginError() throws Exception {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body"));
		HttpServletRequest request = twoFactorCodeRequest("123456");
		HttpServletResponse response = responseWithRedirect();
		FilterChain chain = mock(FilterChain.class);
		AuthenticationManager authenticationManager = mock(AuthenticationManager.class);
		filter.setAuthenticationManager(authenticationManager);
		when(authenticationManager.authenticate(any(Authentication.class))).thenThrow(new LockedException("locked"));

		filter.doFilter(request, response, chain);

		verify(response).sendRedirect("/login?error");
		verify(request, never()).setAttribute(eq(TwoFactorAuthForwardHandler.TWO_FACTOR_AUTH_ERROR_ATTRIBUTE), any());
		verify(chain, never()).doFilter(any(), any());
	}

	@Test
	public void testLockedUserCannotStartTwoFactorChallenge() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameter("password")).thenReturn("secret");

		assertFalse(filter.callCanAuthenticateWithPassword(request, twoFactorUser(false)));
	}

	private static HttpServletRequest loginRequest(String customer) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletMapping mapping = mock(HttpServletMapping.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getMethod()).thenReturn("POST");
		when(request.getServletPath()).thenReturn(SkyveSpringSecurity.LOGIN_ATTEMPT_PATH);
		when(request.getPathInfo()).thenReturn(null);
		when(request.getContextPath()).thenReturn("");
		when(request.getRequestURI()).thenReturn(SkyveSpringSecurity.LOGIN_ATTEMPT_PATH);
		when(mapping.getMappingMatch()).thenReturn(MappingMatch.PATH);
		when(mapping.getPattern()).thenReturn("/*");
		when(mapping.getMatchValue()).thenReturn(SkyveSpringSecurity.LOGIN_ATTEMPT_PATH);
		when(mapping.getServletName()).thenReturn("dispatcher");
		when(request.getHttpServletMapping()).thenReturn(mapping);
		when(request.getParameter(TwoFactorAuthPushFilter.SKYVE_SECURITY_FORM_CUSTOMER_KEY)).thenReturn(customer);
		when(request.getSession()).thenReturn(session);
		when(request.getSession(anyBoolean())).thenReturn(session);
		return request;
	}

	private HttpServletRequest twoFactorCodeRequest(String code) {
		HttpServletRequest request = loginRequest(CUSTOMER);
		String token = "token-" + System.currentTimeMillis();
		when(request.getParameter("username")).thenReturn(CUSTOMER + "/bob");
		when(request.getParameter("password")).thenReturn(code);
		when(request.getParameter(TwoFactorAuthPushFilter.TWO_FACTOR_TOKEN_ATTRIBUTE)).thenReturn(token);
		when(userDetailsManager.loadUserByUsername(CUSTOMER + "/bob")).thenReturn(twoFactorUser(true, token));
		return request;
	}

	private static HttpServletResponse responseWithForward(HttpServletRequest request) {
		HttpServletResponse response = mock(HttpServletResponse.class);
		RequestDispatcher dispatcher = mock(RequestDispatcher.class);
		when(response.isCommitted()).thenReturn(false);
		when(request.getRequestDispatcher("/login")).thenReturn(dispatcher);
		return response;
	}

	private static HttpServletResponse responseWithRedirect() {
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.isCommitted()).thenReturn(false);
		when(response.encodeRedirectURL(anyString())).thenAnswer(invocation -> invocation.getArgument(0));
		return response;
	}

	private static TwoFactorAuthUser twoFactorUser(boolean accountNonLocked) {
		return twoFactorUser(accountNonLocked, "token-" + System.currentTimeMillis());
	}

	private static TwoFactorAuthUser twoFactorUser(boolean accountNonLocked, String token) {
		return new TwoFactorAuthUser(CUSTOMER + "/bob",
										"hashed-tfa-code",
										true,
										true,
										true,
										accountNonLocked,
										AuthorityUtils.NO_AUTHORITIES,
										CUSTOMER,
										"bob",
										"hashed-tfa-code",
										token,
										new Timestamp(),
										"bob@example.com",
										"hashed-password");
	}

	@SuppressWarnings("unchecked")
	private static ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration> getConfigurationMap() throws Exception {
		Field field = TwoFactorAuthConfigurationSingleton.class.getDeclaredField("configuration");
		field.setAccessible(true);
		return (ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration>) field.get(TwoFactorAuthConfigurationSingleton.getInstance());
	}

	private static class TestTwoFactorAuthPushFilter extends TwoFactorAuthPushFilter {
		private TestTwoFactorAuthPushFilter(UserDetailsManager userDetailsManager) {
			super(userDetailsManager);
		}

		private boolean callSkipPushFilter(HttpServletRequest request, HttpServletResponse response) {
			return skipPushFilter(request, response);
		}

		private boolean callIsValidTwoFactorCode(String code) {
			return isValidTwoFactorCode(code);
		}

		private boolean callCanAuthenticateWithPassword(HttpServletRequest request, TwoFactorAuthUser user) {
			return canAuthenticateWithPassword(request, user);
		}

		@Override
		protected boolean supportsPushConfiguration(TwoFactorAuthCustomerConfiguration config) {
			return (config != null) && config.isTfaEmail();
		}

		@Override
		protected void pushNotification(TwoFactorAuthUser user, String code) {
			// no-op for test
		}
	}
}
