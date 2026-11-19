package org.skyve.impl.web.spring; // NOSONAR java:S1220

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.util.TwoFactorAuthConfigurationSingleton;
import org.skyve.impl.util.TwoFactorAuthCustomerConfiguration;
import org.skyve.impl.util.UtilImpl;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.config.ObjectPostProcessor;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.provisioning.UserDetailsManager;
import org.springframework.security.web.DefaultSecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.security.web.authentication.rememberme.InMemoryTokenRepositoryImpl;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.http.Cookie;

/**
 * Tests that a two-factor sign in issues the Remember me cookie as a normal sign in does, through the filters that
 * {@link SpringSecurityConfig} builds: the two-factor filter checks the code, then the form login filter that follows it
 * signs the user in with the {@link SkyveRememberMeServices} and so issues (or not) the cookie.
 */
class TwoFactorRememberMeSignInTest {
	private static final String CUSTOMER = "acme";
	private static final String USERNAME = CUSTOMER + "/bob";
	private static final String TOKEN = "token-" + System.currentTimeMillis();
	private static final String CODE = "123456";

	private String originalServerUrl;
	private String originalContext;
	private String originalHomeUri;
	private Set<String> originalTwoFactorCustomers;
	private int originalLockoutThreshold;
	private ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration> configurationMap;
	private Map<String, TwoFactorAuthCustomerConfiguration> originalConfigurations;
	private InMemoryTokenRepositoryImpl tokenRepository;
	@SuppressWarnings("resource") // closed in tearDown
	private GenericApplicationContext applicationContext;
	private Filter twoFactorFilter;
	private Filter formLoginFilter;

	@BeforeEach
	@SuppressWarnings("resource") // the application context is closed in tearDown
	void setUp() throws Exception {
		originalServerUrl = UtilImpl.SERVER_URL;
		originalContext = UtilImpl.SKYVE_CONTEXT;
		originalHomeUri = UtilImpl.HOME_URI;
		originalTwoFactorCustomers = UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS;
		originalLockoutThreshold = UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD;
		UtilImpl.SERVER_URL = "";
		UtilImpl.SKYVE_CONTEXT = "";
		UtilImpl.HOME_URI = "/home";
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = Set.of(CUSTOMER);
		UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD = 0;
		configurationMap = configurationMap();
		originalConfigurations = Map.copyOf(configurationMap);
		configurationMap.clear();
		configurationMap.put(CUSTOMER, new TwoFactorAuthCustomerConfiguration("EMAIL", 300, "subject", "body"));

		TwoFactorAuthUser user = new TwoFactorAuthUser(USERNAME, "ignored", true, true, true, true,
				Collections.emptyList(), CUSTOMER, "bob",
				"hashed-code", TOKEN, new Timestamp(), "bob@skyve.org", "hashed-password");
		UserDetailsManager userDetailsManager = mock(UserDetailsManager.class);
		when(userDetailsManager.loadUserByUsername(USERNAME)).thenReturn(user);
		tokenRepository = new InMemoryTokenRepositoryImpl();
		SkyveSpringSecurity skyve = mock(SkyveSpringSecurity.class);
		when(skyve.jdbcUserDetailsManager()).thenReturn(userDetailsManager);
		when(skyve.tokenRepository()).thenReturn(tokenRepository);
		when(skyve.authenticationDetailsSource()).thenReturn(new WebAuthenticationDetailsSource());

		// The code is the password of the second step: only the right one signs in
		AuthenticationManager authenticationManager = authentication -> {
			if (!CODE.equals(authentication.getCredentials())) {
				throw new BadCredentialsException("bad code");
			}
			return UsernamePasswordAuthenticationToken.authenticated(user, CODE, user.getAuthorities());
		};

		ObjectPostProcessor<Object> objectPostProcessor = new ObjectPostProcessor<>() {
			@Override
			public <O extends Object> O postProcess(O object) {
				return object;
			}
		};
		Map<Class<?>, Object> sharedObjects = new HashMap<>();
		applicationContext = new GenericApplicationContext();
		applicationContext.refresh();
		sharedObjects.put(org.springframework.context.ApplicationContext.class, applicationContext);
		sharedObjects.put(UserDetailsService.class, userDetailsManager);
		HttpSecurity http = new HttpSecurity(objectPostProcessor, new AuthenticationManagerBuilder(objectPostProcessor),
				sharedObjects);
		http.authenticationManager(authenticationManager);
		DefaultSecurityFilterChain chain = (DefaultSecurityFilterChain) new SpringSecurityConfig(skyve).filterChain(http);
		twoFactorFilter = chain.getFilters()
				.stream()
				.filter(TwoFactorAuthPushEmailFilter.class::isInstance)
				.findFirst()
				.orElseThrow();
		formLoginFilter = chain.getFilters()
				.stream()
				.filter(f -> f.getClass() == UsernamePasswordAuthenticationFilter.class)
				.findFirst()
				.orElseThrow();
	}

	@AfterEach
	void tearDown() {
		applicationContext.close();
		UtilImpl.SERVER_URL = originalServerUrl;
		UtilImpl.SKYVE_CONTEXT = originalContext;
		UtilImpl.HOME_URI = originalHomeUri;
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = originalTwoFactorCustomers;
		UtilImpl.ACCOUNT_LOCKOUT_THRESHOLD = originalLockoutThreshold;
		configurationMap.clear();
		configurationMap.putAll(originalConfigurations);
	}

	@Test
	void aGoodCodeWithRememberMeTickedIssuesTheRememberMeCookie() throws Exception {
		MockHttpServletResponse response = submitCode(CODE, true);

		assertEquals("/home", response.getRedirectedUrl(), "the sign in succeeded");
		Cookie cookie = response.getCookie("remember");
		assertNotNull(cookie, "a Remember me cookie is issued");
		assertTrue(cookie.getMaxAge() > 0, "it outlives the browser session");
		assertTrue(cookie.isHttpOnly());
		assertNotNull(tokenRepository.getTokenForSeries(series(cookie)), "its token series is stored for the user");
		assertEquals(USERNAME, tokenRepository.getTokenForSeries(series(cookie)).getUsername());
	}

	@Test
	void aGoodCodeWithRememberMeUntickedIssuesNoCookie() throws Exception {
		MockHttpServletResponse response = submitCode(CODE, false);

		assertEquals("/home", response.getRedirectedUrl(), "the sign in succeeded");
		assertNull(response.getCookie("remember"));
	}

	@Test
	void aBadCodeWithRememberMeTickedIssuesNothing() throws Exception {
		MockHttpServletResponse response = submitCode("000000", true);

		assertNull(response.getCookie("remember"));
		assertNull(response.getRedirectedUrl(), "not signed in");
		assertEquals("/login", response.getForwardedUrl(), "the code page is shown again");
	}

	private MockHttpServletResponse submitCode(String code, boolean remember) throws Exception {
		MockHttpServletRequest request = new MockHttpServletRequest("POST", SkyveSpringSecurity.LOGIN_ATTEMPT_PATH);
		request.setServletPath(SkyveSpringSecurity.LOGIN_ATTEMPT_PATH);
		request.getSession(true);
		request.setParameter("username", USERNAME);
		request.setParameter("customer", CUSTOMER);
		request.setParameter("password", code);
		request.setParameter("tfaToken", TOKEN);
		if (remember) {
			request.setParameter("remember", "on");
		}
		MockHttpServletResponse response = new MockHttpServletResponse();
		AtomicInteger reachedTheEnd = new AtomicInteger();
		FilterChain end = (rq, rs) -> reachedTheEnd.incrementAndGet();
		twoFactorFilter.doFilter(request, response, (rq, rs) -> formLoginFilter.doFilter(rq, rs, end));
		return response;
	}

	private static String series(Cookie cookie) {
		String decoded = new String(java.util.Base64.getDecoder().decode(cookie.getValue()),
				java.nio.charset.StandardCharsets.UTF_8);
		return java.net.URLDecoder.decode(decoded.split(":")[0], java.nio.charset.StandardCharsets.UTF_8);
	}

	@SuppressWarnings("unchecked")
	private static ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration> configurationMap()
			throws ReflectiveOperationException {
		Field field = TwoFactorAuthConfigurationSingleton.class.getDeclaredField("configuration");
		field.setAccessible(true);
		return (ConcurrentHashMap<String, TwoFactorAuthCustomerConfiguration>) field
				.get(TwoFactorAuthConfigurationSingleton.getInstance());
	}
}
