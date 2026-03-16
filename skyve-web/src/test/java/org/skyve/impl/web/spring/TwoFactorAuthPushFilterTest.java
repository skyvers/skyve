package org.skyve.impl.web.spring;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
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
import org.skyve.impl.util.TwoFactorAuthConfigurationSingleton;
import org.skyve.impl.util.TwoFactorAuthCustomerConfiguration;
import org.skyve.impl.util.UtilImpl;
import org.springframework.security.provisioning.UserDetailsManager;

import jakarta.servlet.FilterChain;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

public class TwoFactorAuthPushFilterTest {
	private static final String CUSTOMER = "acme";
	private Set<String> previousTwoFactorCustomers;
	private Map<String, TwoFactorAuthCustomerConfiguration> previousConfigurations;
	private ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration> configurationMap;
	private TestTwoFactorAuthPushFilter filter;

	@BeforeEach
	public void setUp() throws Exception {
		previousTwoFactorCustomers = UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS;
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = Set.of(CUSTOMER);

		configurationMap = getConfigurationMap();
		previousConfigurations = Map.copyOf(configurationMap);
		configurationMap.clear();

		filter = new TestTwoFactorAuthPushFilter(mock(UserDetailsManager.class));
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
	public void testSkipPushFilterWhenTwoFactorIsUnsupportedSms() {
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("SMS", 300, "subject", "body"));
		boolean skip = filter.callSkipPushFilter(loginRequest(CUSTOMER), mock(HttpServletResponse.class));
		assertFalse(skip);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testIsPushTfaRemainsEmailOnly() {
		assertFalse(TwoFactorAuthConfigurationSingleton
				.isPushTfa(new TwoFactorAuthCustomerConfiguration("OFF", 300, "subject", "body")));
		assertTrue(TwoFactorAuthConfigurationSingleton
				.isPushTfa(new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body")));
		assertFalse(TwoFactorAuthConfigurationSingleton
				.isPushTfa(new TwoFactorAuthCustomerConfiguration("SMS", 300, "subject", "body")));
	}

	@Test
	@SuppressWarnings("boxing")
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

	private static HttpServletRequest loginRequest(String customer) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getMethod()).thenReturn("POST");
		when(request.getServletPath()).thenReturn(SkyveSpringSecurity.LOGIN_ATTEMPT_PATH);
		when(request.getPathInfo()).thenReturn(null);
		when(request.getContextPath()).thenReturn("");
		when(request.getRequestURI()).thenReturn(SkyveSpringSecurity.LOGIN_ATTEMPT_PATH);
		when(request.getParameter(TwoFactorAuthPushFilter.SKYVE_SECURITY_FORM_CUSTOMER_KEY)).thenReturn(customer);
		when(request.getSession()).thenReturn(session);
		when(request.getSession(anyBoolean())).thenReturn(session);
		return request;
	}

	@SuppressWarnings("unchecked")
	private static ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration> getConfigurationMap() throws Exception {
		Field field = TwoFactorAuthConfigurationSingleton.class.getDeclaredField("configuration");
		field.setAccessible(true);
		return (ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration>) field
				.get(TwoFactorAuthConfigurationSingleton.getInstance());
	}

	private static class TestTwoFactorAuthPushFilter extends TwoFactorAuthPushFilter {
		private TestTwoFactorAuthPushFilter(UserDetailsManager userDetailsManager) {
			super(userDetailsManager);
		}

		private boolean callSkipPushFilter(HttpServletRequest request, HttpServletResponse response) {
			return skipPushFilter(request, response);
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
