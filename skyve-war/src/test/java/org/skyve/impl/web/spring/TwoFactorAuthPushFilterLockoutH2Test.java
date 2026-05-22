package org.skyve.impl.web.spring;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.util.TwoFactorAuthConfigurationSingleton;
import org.skyve.impl.util.TwoFactorAuthCustomerConfiguration;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.SecurityUtil;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.DefaultAuthenticationEventPublisher;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.authentication.event.AbstractAuthenticationFailureEvent;
import org.springframework.security.authentication.event.AuthenticationSuccessEvent;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.provisioning.UserDetailsManager;

import jakarta.servlet.FilterChain;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.http.HttpServletMapping;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import jakarta.servlet.http.MappingMatch;
import util.AbstractH2Test;

class TwoFactorAuthPushFilterLockoutH2Test extends AbstractH2Test {
	private Set<String> previousTwoFactorCustomers;
	private int previousAccountLockoutThreshold;
	private int previousAccountLockoutDurationMultipleInSeconds;
	private Map<String, TwoFactorAuthCustomerConfiguration> previousConfigurations;
	private ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration> configurationMap;

	@BeforeEach
	void beforeEach() throws Exception {
		previousTwoFactorCustomers = UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS;
		previousAccountLockoutThreshold = UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD;
		previousAccountLockoutDurationMultipleInSeconds = UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS;

		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = Set.of(CUSTOMER);
		UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD = 5;
		UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS = 10;

		configurationMap = getConfigurationMap();
		previousConfigurations = Map.copyOf(configurationMap);
		configurationMap.clear();
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body"));
	}

	@AfterEach
	void afterEach() {
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = previousTwoFactorCustomers;
		UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD = previousAccountLockoutThreshold;
		UtilImpl.ACCOUNT_LOCKOUT_DURATION_MULTIPLE_IN_SECONDS = previousAccountLockoutDurationMultipleInSeconds;
		configurationMap.clear();
		configurationMap.putAll(previousConfigurations);
	}

	@Test
	void testBadTwoFactorCodeAtThresholdLocksAccountInSameAttempt() throws Exception {
		String username = "lockout.user." + UUID.randomUUID().toString().replace("-", "").substring(0, 12);
		String fullUsername = CUSTOMER + "/" + username;
		String token = "valid-" + System.currentTimeMillis();
		insertSecurityUserWithTwoFactorState(username, token, 4);

		UserDetailsManager userDetailsManager = new SkyveSpringSecurity().jdbcUserDetailsManager();
		TwoFactorAuthPushEmailFilter filter = new FreshLookupTwoFactorAuthPushEmailFilter(userDetailsManager);
		AuthenticationManager authenticationManager = buildAuthenticationManager(userDetailsManager);
		filter.setAuthenticationManager(authenticationManager);

		HttpServletRequest request = tfaCodeRequest(token, fullUsername, "123456");
		RequestDispatcher dispatcher = mock(RequestDispatcher.class);
		when(request.getRequestDispatcher("/login")).thenReturn(dispatcher);
		HttpServletResponse response = loginResponse();
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		UserDetails updated = userDetailsManager.loadUserByUsername(fullUsername);
		verifyAccountLocked(updated);
		verify(chain, never()).doFilter(any(), any());
	}

	private static final class FreshLookupTwoFactorAuthPushEmailFilter extends TwoFactorAuthPushEmailFilter {
		private FreshLookupTwoFactorAuthPushEmailFilter(UserDetailsManager userDetailsManager) {
			super(userDetailsManager);
		}

		@Override
		protected TwoFactorAuthUser getUserDB(String username) {
			UserDetailsManager freshManager = new SkyveSpringSecurity().jdbcUserDetailsManager();
			UserDetails userDetails = freshManager.loadUserByUsername(username);
			return userDetails instanceof TwoFactorAuthUser twoFactorAuthUser ? twoFactorAuthUser : null;
		}
	}

	private static AuthenticationManager buildAuthenticationManager(UserDetailsManager userDetailsManager) {
		DaoAuthenticationProvider provider = new DaoAuthenticationProvider();
		provider.setUserDetailsService(userDetailsManager);
		provider.setPasswordEncoder(SecurityUtil.createDelegatingPasswordEncoder());

		ProviderManager manager = new ProviderManager(provider);
		SecurityListener listener = new SecurityListener();
		ApplicationEventPublisher publisher = new ApplicationEventPublisher() {
			@Override
			public void publishEvent(ApplicationEvent event) {
				if (event instanceof AbstractAuthenticationFailureEvent failureEvent) {
					listener.onAuthenticationFailure(failureEvent);
				}
				else if (event instanceof AuthenticationSuccessEvent successEvent) {
					listener.onAuthenticationSuccess(successEvent);
				}
			}

			@Override
			public void publishEvent(Object event) {
				if (event instanceof ApplicationEvent appEvent) {
					publishEvent(appEvent);
				}
			}
		};
		manager.setAuthenticationEventPublisher(new DefaultAuthenticationEventPublisher(publisher));
		return manager;
	}

	private static void verifyAccountLocked(UserDetails details) {
		if (details.isAccountNonLocked()) {
			throw new AssertionError("Expected account to be locked after threshold-crossing MFA failure.");
		}
	}

	private static void insertSecurityUserWithTwoFactorState(String username, String token, int authenticationFailures)
	throws SQLException {
		String contactId = UUID.randomUUID().toString();
		String userId = UUID.randomUUID().toString();
		long now = new DateTime().getTime();
		String email = username + "@skyve.org";
		String validTwoFactorCode = "654321";
		Timestamp twoFactorCodeGeneratedTimestamp = new Timestamp(now);
		Timestamp lastAuthenticationFailure = new Timestamp(now);

		try (Connection c = EXT.getDataStoreConnection();
				PreparedStatement contactInsert = c.prepareStatement(
						"insert into ADM_Contact (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId, email1) values (?, ?, ?, ?, ?, ?, ?)");
				PreparedStatement userInsert = c.prepareStatement(
						"insert into ADM_SecurityUser (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId, userName, password, contact_id, inactive, activated, twoFactorCode, twoFactorToken, twoFactorCodeGeneratedTimestamp, authenticationFailures, lastAuthenticationFailure) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")) {
			c.setAutoCommit(true);

			contactInsert.setString(1, contactId);
			contactInsert.setInt(2, 0);
			contactInsert.setString(3, "0");
			contactInsert.setString(4, username);
			contactInsert.setString(5, CUSTOMER);
			contactInsert.setString(6, USER);
			contactInsert.setString(7, email);
			contactInsert.executeUpdate();

			userInsert.setString(1, userId);
			userInsert.setInt(2, 0);
			userInsert.setString(3, "0");
			userInsert.setString(4, username);
			userInsert.setString(5, CUSTOMER);
			userInsert.setString(6, USER);
			userInsert.setString(7, username);
			userInsert.setString(8, EXT.hashPassword(PASSWORD));
			userInsert.setString(9, contactId);
			userInsert.setBoolean(10, false);
			userInsert.setBoolean(11, true);
			userInsert.setString(12, EXT.hashPassword(validTwoFactorCode));
			userInsert.setString(13, token);
			userInsert.setTimestamp(14, twoFactorCodeGeneratedTimestamp);
			userInsert.setInt(15, authenticationFailures);
			userInsert.setTimestamp(16, lastAuthenticationFailure);
			userInsert.executeUpdate();
		}
	}

	@SuppressWarnings("unchecked")
	private static ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration> getConfigurationMap() throws Exception {
		Field field = TwoFactorAuthConfigurationSingleton.class.getDeclaredField("configuration");
		field.setAccessible(true);
		return (ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration>) field.get(TwoFactorAuthConfigurationSingleton.getInstance());
	}

	private static HttpServletRequest tfaCodeRequest(String token, String username, String code) {
		HttpServletRequest request = loginRequest(CUSTOMER);
		when(request.getParameter("username")).thenReturn(username);
		when(request.getParameter("password")).thenReturn(code);
		when(request.getParameter(TwoFactorAuthPushFilter.TWO_FACTOR_TOKEN_ATTRIBUTE)).thenReturn(token);
		return request;
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
		when(response.isCommitted()).thenReturn(false);
		when(response.encodeRedirectURL(anyString())).thenAnswer(invocation -> invocation.getArgument(0));
		return response;
	}
}
