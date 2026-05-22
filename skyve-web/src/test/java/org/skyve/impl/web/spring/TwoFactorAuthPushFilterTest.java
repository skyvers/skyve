package org.skyve.impl.web.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
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
import java.util.Collections;
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

class TwoFactorAuthPushFilterTest {
	private static final String CUSTOMER = "acme";
	private static final String USER = "test.user";
	private static final String USERNAME = CUSTOMER + "/" + USER;

	private Set<String> previousTwoFactorCustomers;
	private int previousResendCooldownSeconds;
	private Map<String, TwoFactorAuthCustomerConfiguration> previousConfigurations;
	private ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration> configurationMap;
	private UserDetailsManager userDetailsManager;
	private TestTwoFactorAuthPushFilter filter;

	@BeforeEach
	void setUp() throws Exception {
		previousTwoFactorCustomers = UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS;
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = Set.of(CUSTOMER);
		previousResendCooldownSeconds = UtilImpl.TWO_FACTOR_AUTH_RESEND_COOLDOWN_SECONDS;
		UtilImpl.TWO_FACTOR_AUTH_RESEND_COOLDOWN_SECONDS = 60;

		configurationMap = getConfigurationMap();
		previousConfigurations = Map.copyOf(configurationMap);
		configurationMap.clear();

		userDetailsManager = mock(UserDetailsManager.class);
		filter = new TestTwoFactorAuthPushFilter(userDetailsManager);
	}

	@AfterEach
	void tearDown() {
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = previousTwoFactorCustomers;
		UtilImpl.TWO_FACTOR_AUTH_RESEND_COOLDOWN_SECONDS = previousResendCooldownSeconds;
		configurationMap.clear();
		configurationMap.putAll(previousConfigurations);
	}

	@Test
	void testSkipPushFilterWhenTwoFactorIsOff() {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("OFF", 300, "subject", "body"));
		boolean skip = filter.callSkipPushFilter(loginRequest(CUSTOMER), mock(HttpServletResponse.class));
		assertTrue(skip);
	}

	@Test
	void testSkipPushFilterWhenTwoFactorIsEmail() {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body"));
		boolean skip = filter.callSkipPushFilter(loginRequest(CUSTOMER), mock(HttpServletResponse.class));
		assertFalse(skip);
	}

	@Test
	void testSkipPushFilterWhenTwoFactorIsUnsupportedFactor() {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("SMS", 300, "subject", "body"));
		boolean skip = filter.callSkipPushFilter(loginRequest(CUSTOMER), mock(HttpServletResponse.class));
		assertFalse(skip);
	}

	@Test
	void testIsPushTfaRemainsEmailOnly() {
		assertFalse(TwoFactorAuthConfigurationSingleton.isPushTfa(new TwoFactorAuthCustomerConfiguration("OFF", 300, "subject", "body")));
		assertTrue(TwoFactorAuthConfigurationSingleton.isPushTfa(new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body")));
		assertFalse(TwoFactorAuthConfigurationSingleton.isPushTfa(new TwoFactorAuthCustomerConfiguration("SMS", 300, "subject", "body")));
	}

	@Test
	void testUnsupportedFactorDoesNotBypassMfa() throws Exception {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("SMS", 300, "subject", "body"));

		HttpServletRequest request = loginRequest(CUSTOMER);
		HttpServletResponse response = loginResponse();

		FilterChain chain = mock(FilterChain.class);
		filter.doFilter(request, response, chain);

		verify(chain, never()).doFilter(any(), any());
		verify(response).sendRedirect("/login?error");
	}

	@Test
	void testTwoFactorCodeValidationAcceptsStrictSixDigits() {
		assertTrue(filter.callIsValidTwoFactorCode("123456"));
	}

	@Test
	void testTwoFactorCodeValidationRejectsNonSixDigitValues() {
		assertFalse(filter.callIsValidTwoFactorCode(null));
		assertFalse(filter.callIsValidTwoFactorCode(""));
		assertFalse(filter.callIsValidTwoFactorCode("12345"));
		assertFalse(filter.callIsValidTwoFactorCode("1234567"));
		assertFalse(filter.callIsValidTwoFactorCode("12a456"));
	}

	@Test
	void testResendCooldownRejectsWhenTooSoon() {
		TwoFactorAuthUser user = createUser("token-now", new Timestamp(System.currentTimeMillis() - 10_000L));
		assertTrue(filter.callIsResendOnCooldown(user));
	}

	@Test
	void testResendCooldownAllowsAfterElapsed() {
		TwoFactorAuthUser user = createUser("token-old", new Timestamp(System.currentTimeMillis() - 120_000L));
		assertFalse(filter.callIsResendOnCooldown(user));
	}

	@Test
	void testResendCooldownAllowsWhenNoTimestamp() {
		TwoFactorAuthUser user = createUser("token-none", null);
		assertFalse(filter.callIsResendOnCooldown(user));
	}

	@Test
	void testResendCooldownUsesConfiguredThreshold() {
		UtilImpl.TWO_FACTOR_AUTH_RESEND_COOLDOWN_SECONDS = 180;
		TwoFactorAuthUser user = createUser("token-config", new Timestamp(System.currentTimeMillis() - 120_000L));
		assertTrue(filter.callIsResendOnCooldown(user));
	}

	@Test
	void testResendCooldownUsesExactCurrentTimeMillis() {
		UtilImpl.TWO_FACTOR_AUTH_RESEND_COOLDOWN_SECONDS = 60;
		Timestamp generated = new Timestamp(1_000L);
		TwoFactorAuthUser user = createUser("token-exact", generated);
		filter.setCurrentTimeMillisOverride(61_000L);
		assertFalse(filter.callIsResendOnCooldown(user));
	}

	@Test
	void testResendSuccessHaltsChainAndForwards() throws Exception {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body"));
		String token = "valid-" + System.currentTimeMillis();
		TwoFactorAuthUser user = createUser(token, new Timestamp(System.currentTimeMillis() - 120_000L));
		filter.setUserToReturn(user);
		filter.setExpiredOverride(Boolean.FALSE);

		HttpServletRequest request = resendRequest(token, true);
		RequestDispatcher dispatcher = mock(RequestDispatcher.class);
		when(request.getRequestDispatcher("/login")).thenReturn(dispatcher);
		HttpServletResponse response = loginResponse();
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(chain, never()).doFilter(any(), any());
		assertTrue(filter.pushNotificationCalled);
		assertEquals(1, filter.pushNotificationCallCount);
		verify(request).setAttribute(TwoFactorAuthPushFilter.RESEND_SUCCESS_ATTRIBUTE, Boolean.TRUE);
		verify(dispatcher).forward(request, response);
	}

	@Test
	void testResendOnCooldownHaltsChainAndForwards() throws Exception {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body"));
		String token = "valid-" + System.currentTimeMillis();
		TwoFactorAuthUser user = createUser(token, new Timestamp(System.currentTimeMillis() - 10_000L));
		filter.setUserToReturn(user);
		filter.setExpiredOverride(Boolean.FALSE);

		HttpServletRequest request = resendRequest(token, false);
		RequestDispatcher dispatcher = mock(RequestDispatcher.class);
		when(request.getRequestDispatcher("/login")).thenReturn(dispatcher);
		HttpServletResponse response = loginResponse();
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(chain, never()).doFilter(any(), any());
		assertFalse(filter.pushNotificationCalled);
		verify(request).setAttribute(TwoFactorAuthPushFilter.RESEND_COOLDOWN_ATTRIBUTE, Boolean.TRUE);
		verify(dispatcher).forward(request, response);
	}

	@Test
	void testResendWithExpiredTokenHaltsChainAndRedirects() throws Exception {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body"));
		String token = "valid-" + System.currentTimeMillis();
		TwoFactorAuthUser user = createUser(token, new Timestamp(System.currentTimeMillis() - 120_000L));
		filter.setUserToReturn(user);
		filter.setExpiredOverride(Boolean.TRUE);

		HttpServletRequest request = resendRequest(token, true);
		HttpServletResponse response = loginResponse();
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(chain, never()).doFilter(any(), any());
		assertFalse(filter.pushNotificationCalled);
		verify(response).sendRedirect("/login?" + TwoFactorAuthPushFilter.TWO_FACTOR_EXPIRED_PARAMETER);
	}

	@Test
	void testResendWithMismatchedTokenHaltsChainAndRedirects() throws Exception {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body"));
		TwoFactorAuthUser user = createUser("server-token", new Timestamp(System.currentTimeMillis() - 120_000L));
		filter.setUserToReturn(user);
		filter.setExpiredOverride(Boolean.FALSE);

		HttpServletRequest request = resendRequest("client-token", true);
		HttpServletResponse response = loginResponse();
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(chain, never()).doFilter(any(), any());
		assertFalse(filter.pushNotificationCalled);
		verify(response).sendRedirect("/login");
	}

	@Test
	void testResendInvalidatesOldCode() throws Exception {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body"));
		String originalToken = "valid-" + System.currentTimeMillis();
		Timestamp originalTimestamp = new Timestamp(System.currentTimeMillis() - 120_000L);
		TwoFactorAuthUser user = createUser(originalToken, originalTimestamp);
		String originalCodeHash = user.getTfaCode();

		filter.setUserToReturn(user);
		filter.setExpiredOverride(Boolean.FALSE);
		filter.generatedCode = "654321";
		filter.generatedPushId = "new-token";

		HttpServletRequest request = resendRequest(originalToken, false);
		RequestDispatcher dispatcher = mock(RequestDispatcher.class);
		when(request.getRequestDispatcher("/login")).thenReturn(dispatcher);
		HttpServletResponse response = loginResponse();
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(chain, never()).doFilter(any(), any());
		assertTrue(filter.updateUserCalled);
		assertNotEquals(originalCodeHash, user.getTfaCode());
		assertNotEquals(originalToken, user.getTfaToken());
		assertNotEquals(originalTimestamp.getTime(), user.getTfaCodeGeneratedTimestamp().getTime());
	}

	private static TwoFactorAuthUser createUser(String token, Timestamp timestamp) {
		return new TwoFactorAuthUser(USERNAME,
								"ignored",
								true,
								true,
								true,
								true,
								Collections.emptyList(),
								CUSTOMER,
								USER,
								"existing-hash",
								token,
								timestamp,
								"to@skyve.org",
								"hashed-password");
	}

	private static HttpServletRequest resendRequest(String token, boolean rememberMe) {
		HttpServletRequest request = loginRequest(CUSTOMER);
		when(request.getParameter("username")).thenReturn(USERNAME);
		when(request.getParameter("user")).thenReturn(USER);
		when(request.getParameter(TwoFactorAuthPushFilter.TWO_FACTOR_TOKEN_ATTRIBUTE)).thenReturn(token);
		when(request.getParameter(TwoFactorAuthPushFilter.RESEND_ATTRIBUTE)).thenReturn("true");
		when(request.getParameter(TwoFactorAuthPushFilter.REMEMBER_PARAMETER)).thenReturn(rememberMe ? "true" : null);
		return request;
	}

	@Test
	void testInvalidTwoFactorFormatStillAttemptsAuthentication() throws Exception {
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
	void testBadTwoFactorCodeStaysOnTwoFactorPage() throws Exception {
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
	void testExpiredTwoFactorCodeRedirectsToExpiredLoginMessage() throws Exception {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body"));
		HttpServletRequest request = twoFactorCodeRequest("123456");
		HttpServletResponse response = responseWithRedirect();
		FilterChain chain = mock(FilterChain.class);
		AuthenticationManager authenticationManager = mock(AuthenticationManager.class);
		filter.setAuthenticationManager(authenticationManager);
		filter.setExpiredOverride(Boolean.TRUE);

		filter.doFilter(request, response, chain);

		verify(authenticationManager, never()).authenticate(any(Authentication.class));
		verify(response).sendRedirect("/login?" + TwoFactorAuthPushFilter.TWO_FACTOR_EXPIRED_PARAMETER);
		verify(request, never()).setAttribute(eq(TwoFactorAuthForwardHandler.TWO_FACTOR_AUTH_ERROR_ATTRIBUTE), any());
		verify(chain, never()).doFilter(any(), any());
	}

	@Test
	void testLockedTwoFactorAttemptRedirectsToLoginError() throws Exception {
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
	void testLockedUserCannotStartTwoFactorChallenge() {
		assertCannotStartTwoFactorChallenge(twoFactorUser(true, true, true, false));
	}

	@Test
	void testDisabledUserCannotStartTwoFactorChallenge() {
		assertCannotStartTwoFactorChallenge(twoFactorUser(false, true, true, true));
	}

	@Test
	void testExpiredUserCannotStartTwoFactorChallenge() {
		assertCannotStartTwoFactorChallenge(twoFactorUser(true, false, true, true));
	}

	@Test
	void testCredentialsExpiredUserCannotStartTwoFactorChallenge() {
		assertCannotStartTwoFactorChallenge(twoFactorUser(true, true, false, true));
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

	private static HttpServletResponse loginResponse() throws Exception {
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.isCommitted()).thenReturn(Boolean.FALSE);
		when(response.encodeRedirectURL(anyString())).thenAnswer(invocation -> invocation.getArgument(0));
		return response;
	}

	private HttpServletRequest twoFactorCodeRequest(String code) {
		HttpServletRequest request = loginRequest(CUSTOMER);
		String token = "token-" + System.currentTimeMillis();
		TwoFactorAuthUser user = twoFactorUser(true, token);
		when(request.getParameter("username")).thenReturn(CUSTOMER + "/bob");
		when(request.getParameter("password")).thenReturn(code);
		when(request.getParameter(TwoFactorAuthPushFilter.TWO_FACTOR_TOKEN_ATTRIBUTE)).thenReturn(token);
		when(userDetailsManager.loadUserByUsername(CUSTOMER + "/bob")).thenReturn(user);
		filter.setUserToReturn(user);
		return request;
	}

	private static HttpServletResponse responseWithForward(HttpServletRequest request) {
		HttpServletResponse response = mock(HttpServletResponse.class);
		RequestDispatcher dispatcher = mock(RequestDispatcher.class);
		when(response.isCommitted()).thenReturn(Boolean.FALSE);
		when(request.getRequestDispatcher("/login")).thenReturn(dispatcher);
		return response;
	}

	private static HttpServletResponse responseWithRedirect() {
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.isCommitted()).thenReturn(Boolean.FALSE);
		when(response.encodeRedirectURL(anyString())).thenAnswer(invocation -> invocation.getArgument(0));
		return response;
	}

	private static TwoFactorAuthUser twoFactorUser(boolean accountNonLocked) {
		return twoFactorUser(true, true, true, accountNonLocked);
	}

	private static TwoFactorAuthUser twoFactorUser(boolean accountNonLocked, String token) {
		return twoFactorUser(true, true, true, accountNonLocked, token);
	}

	private static TwoFactorAuthUser twoFactorUser(boolean enabled,
													boolean accountNonExpired,
													boolean credentialsNonExpired,
													boolean accountNonLocked) {
		return twoFactorUser(enabled,
								accountNonExpired,
								credentialsNonExpired,
								accountNonLocked,
								"token-" + System.currentTimeMillis());
	}

	private static TwoFactorAuthUser twoFactorUser(boolean enabled,
													boolean accountNonExpired,
													boolean credentialsNonExpired,
													boolean accountNonLocked,
													String token) {
		return new TwoFactorAuthUser(CUSTOMER + "/bob",
										"hashed-tfa-code",
										enabled,
										accountNonExpired,
										credentialsNonExpired,
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

	private void assertCannotStartTwoFactorChallenge(TwoFactorAuthUser user) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameter("password")).thenReturn("secret");

		assertFalse(filter.callCanAuthenticateWithPassword(request, user));
	}

	@SuppressWarnings("unchecked")
	private static ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration> getConfigurationMap() throws Exception {
		Field field = TwoFactorAuthConfigurationSingleton.class.getDeclaredField("configuration");
		field.setAccessible(true);
		return (ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration>) field.get(TwoFactorAuthConfigurationSingleton.getInstance());
	}

	private static class TestTwoFactorAuthPushFilter extends TwoFactorAuthPushFilter {
		private TwoFactorAuthUser userToReturn;
		private boolean pushNotificationCalled;
		private int pushNotificationCallCount;
		private boolean updateUserCalled;
		private int updateUserCallCount;
		private Boolean expiredOverride;
		private String generatedCode = "123456";
		private String generatedPushId = "generated-token";
		private long currentTimeMillisOverride = Long.MIN_VALUE;

		private TestTwoFactorAuthPushFilter(UserDetailsManager userDetailsManager) {
			super(userDetailsManager);
		}

		private boolean callSkipPushFilter(HttpServletRequest request, HttpServletResponse response) {
			return skipPushFilter(request, response);
		}

		private boolean callIsValidTwoFactorCode(String code) {
			return isValidTwoFactorCode(code);
		}

		private boolean callIsResendOnCooldown(TwoFactorAuthUser user) {
			return isResendOnCooldown(user);
		}

		private void setUserToReturn(TwoFactorAuthUser user) {
			this.userToReturn = user;
		}

		private void setExpiredOverride(Boolean expiredOverride) {
			this.expiredOverride = expiredOverride;
		}

		private void setCurrentTimeMillisOverride(long currentTimeMillisOverride) {
			this.currentTimeMillisOverride = currentTimeMillisOverride;
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
			pushNotificationCalled = true;
			pushNotificationCallCount++;
		}

		@Override
		protected TwoFactorAuthUser getUserDB(String username) {
			return userToReturn;
		}

		@Override
		protected void updateUserTFADetails(TwoFactorAuthUser user) {
			updateUserCalled = true;
			updateUserCallCount++;
		}

		@Override
		protected String generateTFACode() {
			return generatedCode;
		}

		@Override
		protected String generateTFAPushId(Timestamp generatedTS) {
			return generatedPushId;
		}

		@Override
		protected boolean tfaCodeExpired(String customer, String twoFactorCode) {
			if (expiredOverride != null) {
				return expiredOverride.booleanValue();
			}
			return super.tfaCodeExpired(customer, twoFactorCode);
		}

		@Override
		protected long currentTimeMillis() {
			if (currentTimeMillisOverride != Long.MIN_VALUE) {
				return currentTimeMillisOverride;
			}
			return super.currentTimeMillis();
		}
	}
}
