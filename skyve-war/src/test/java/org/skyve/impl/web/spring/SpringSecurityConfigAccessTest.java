package org.skyve.impl.web.spring; // NOSONAR java:S1220

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.mock.web.MockFilterChain;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.security.authentication.AuthenticationDetailsSource;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.ObjectPostProcessor;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextImpl;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.NoOpPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.provisioning.UserDetailsManager;
import org.springframework.security.web.DefaultSecurityFilterChain;
import org.springframework.security.web.FilterChainProxy;
import org.springframework.security.web.authentication.WebAuthenticationDetails;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.Cookie;

/**
 * Who can reach what, through the real Spring Security filter chain Skyve builds: the pages for signing in
 * and resetting a password are open, the pages and servlets behind a login send an anonymous user to the
 * login page, methods other than GET and POST are refused, a sign in changes the session id (so a session
 * id planted before the sign in is useless) and signing out ends the session and its cookie.
 */
class SpringSecurityConfigAccessTest {
	private String serverUrl;
	private String context;
	private String homeUri;
	@SuppressWarnings("resource") // closed in restore
	private GenericApplicationContext applicationContext;
	private FilterChainProxy proxy;

	@BeforeEach
	@SuppressWarnings("resource") // the application context is closed in restore
	void buildTheChain() throws Exception {
		serverUrl = UtilImpl.SERVER_URL;
		context = UtilImpl.SKYVE_CONTEXT;
		homeUri = UtilImpl.HOME_URI;
		UtilImpl.SERVER_URL = "";
		UtilImpl.SKYVE_CONTEXT = "";
		UtilImpl.HOME_URI = "/home";

		SkyveSpringSecurity skyve = mock(SkyveSpringSecurity.class);
		AuthenticationDetailsSource<HttpServletRequest, WebAuthenticationDetails> details = request -> new WebAuthenticationDetails(
				request.getRemoteAddr(), null);
		when(skyve.authenticationDetailsSource()).thenReturn(details);
		UserDetailsManager users = new InMemoryUserDetailsManager(
				User.withUsername("acme/mike").password("secret").authorities(AuthorityUtils.NO_AUTHORITIES).build());
		when(skyve.jdbcUserDetailsManager()).thenReturn(users);
		PersistentTokenRepository tokens = mock(PersistentTokenRepository.class);
		when(skyve.tokenRepository()).thenReturn(tokens);

		DaoAuthenticationProvider provider = new DaoAuthenticationProvider(users);
		provider.setPasswordEncoder(plainText());
		AuthenticationManager authenticationManager = new ProviderManager(provider);

		ObjectPostProcessor<Object> postProcessor = new ObjectPostProcessor<>() {
			@Override
			public <O extends Object> O postProcess(O object) {
				if (object instanceof InitializingBean bean) {
					try {
						bean.afterPropertiesSet();
					} catch (Exception e) {
						throw new IllegalStateException(e);
					}
				}
				return object;
			}
		};
		Map<Class<?>, Object> shared = new HashMap<>();
		applicationContext = new GenericApplicationContext();
		applicationContext.refresh();
		shared.put(ApplicationContext.class, applicationContext);
		shared.put(UserDetailsService.class, users);
		HttpSecurity http = new HttpSecurity(postProcessor, new AuthenticationManagerBuilder(postProcessor), shared);
		http.authenticationManager(authenticationManager);
		// what @EnableWebSecurity applies before the application's own configuration
		http.exceptionHandling(Customizer.withDefaults())
				.securityContext(Customizer.withDefaults())
				.requestCache(Customizer.withDefaults())
				.anonymous(Customizer.withDefaults())
				.servletApi(Customizer.withDefaults());

		DefaultSecurityFilterChain chain = (DefaultSecurityFilterChain) new SpringSecurityConfig(skyve).filterChain(http);
		proxy = new FilterChainProxy(chain);
	}

	@SuppressWarnings("deprecation") // the in-memory user's password is plain text
	private static PasswordEncoder plainText() {
		return NoOpPasswordEncoder.getInstance();
	}

	@AfterEach
	void restore() {
		UtilImpl.SERVER_URL = serverUrl;
		UtilImpl.SKYVE_CONTEXT = context;
		UtilImpl.HOME_URI = homeUri;
		applicationContext.close();
	}

	@Test
	void theLoginAndPasswordResetPagesAreOpenToAnonymousUsers() throws Exception {
		for (String path : List.of("/login", "/loggedOut", "/pages/requestPasswordReset.jsp", "/pages/resetPassword.jsp",
				"/home.jsp", "/health")) {
			Outcome outcome = get("GET", path, null);
			assertTrue(outcome.reachedTheApplication, path + " should be open");
		}
	}

	@Test
	void theChangePasswordPageAndTheDataServletsSendAnAnonymousUserToTheLoginPage() throws Exception {
		for (String path : List.of("/pages/changePassword.jsp", "/smartedit", "/smartlist", "/smartsnap", "/smarttag", "/export",
				"/report", "/chart", "/meta", "/dynamic.png", "/loggedIn.jsp", "/sse/events")) {
			Outcome outcome = get("GET", path, null);
			assertFalse(outcome.reachedTheApplication, path + " must not be reachable anonymously");
			assertEquals("http://localhost/login", outcome.response.getRedirectedUrl(), path);
		}
	}

	@Test
	void aPostToAnythingElseNeedsASignIn() throws Exception {
		Outcome outcome = get("POST", "/anything", null);

		assertFalse(outcome.reachedTheApplication);
		assertEquals("http://localhost/login", outcome.response.getRedirectedUrl());
	}

	@Test
	void methodsOtherThanGetAndPostAreRefusedEvenWhenSignedIn() throws Exception {
		for (String method : List.of("PUT", "DELETE", "PATCH")) {
			Outcome anonymous = get(method, "/anything", null);
			assertFalse(anonymous.reachedTheApplication, method);

			Outcome signedIn = get(method, "/anything", signedInSession());
			assertFalse(signedIn.reachedTheApplication, method);
			assertEquals(403, signedIn.response.getStatus(), method);
		}
	}

	@Test
	void aSignedInUserReachesTheServletsAndThePasswordChangePage() throws Exception {
		for (String path : List.of("/pages/changePassword.jsp", "/smartedit", "/smartlist")) {
			assertTrue(get("GET", path, signedInSession()).reachedTheApplication, path);
		}
		assertTrue(get("POST", "/smartedit", signedInSession()).reachedTheApplication);
	}

	@Test
	void signingInChangesTheSessionIdSoAPlantedSessionIdIsUseless() throws Exception {
		MockHttpSession planted = new MockHttpSession();
		String plantedId = planted.getId();
		MockHttpServletRequest request = new MockHttpServletRequest("POST", "/loginAttempt");
		request.setServletPath("/loginAttempt");
		request.setSession(planted);
		request.setParameter("username", "acme/mike");
		request.setParameter("password", "secret");
		MockHttpServletResponse response = new MockHttpServletResponse();

		proxy.doFilter(request, response, new MockFilterChain());

		assertEquals("/home", response.getRedirectedUrl());
		assertNotEquals(plantedId, request.getSession().getId());
		SecurityContext securityContext = (SecurityContext) request.getSession()
				.getAttribute(HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY);
		assertNotNull(securityContext);
		assertEquals("acme/mike", securityContext.getAuthentication().getName());
	}

	@Test
	void aWrongPasswordGoesBackToTheLoginPageWithAnErrorAndNoSignedInSession() throws Exception {
		MockHttpServletRequest request = new MockHttpServletRequest("POST", "/loginAttempt");
		request.setServletPath("/loginAttempt");
		request.setParameter("username", "acme/mike");
		request.setParameter("password", "not-the-password");
		MockHttpServletResponse response = new MockHttpServletResponse();

		proxy.doFilter(request, response, new MockFilterChain());

		assertEquals("/login?error", response.getRedirectedUrl());
		MockHttpSession session = (MockHttpSession) request.getSession(false);
		assertTrue((session == null)
				|| (session.getAttribute(HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY) == null));
	}

	@Test
	void signingOutEndsTheSessionAndClearsTheSessionCookie() throws Exception {
		MockHttpSession session = signedInSession();
		MockHttpServletRequest request = new MockHttpServletRequest("POST", "/logout");
		request.setServletPath("/logout");
		request.setSession(session);
		MockHttpServletResponse response = new MockHttpServletResponse();

		proxy.doFilter(request, response, new MockFilterChain());

		assertEquals("/loggedOut", response.getRedirectedUrl());
		assertTrue(session.isInvalid());
		Cookie cookie = response.getCookie("JSESSIONID");
		assertNotNull(cookie);
		assertEquals(0, cookie.getMaxAge());
	}

	private static MockHttpSession signedInSession() {
		MockHttpSession result = new MockHttpSession();
		SecurityContext context = new SecurityContextImpl(
				UsernamePasswordAuthenticationToken.authenticated("acme/mike", "secret", AuthorityUtils.NO_AUTHORITIES));
		result.setAttribute(HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY, context);
		return result;
	}

	private Outcome get(String method, String path, MockHttpSession session) throws IOException, ServletException {
		MockHttpServletRequest request = new MockHttpServletRequest(method, path);
		request.setServletPath(path);
		if (session != null) {
			request.setSession(session);
		}
		MockHttpServletResponse response = new MockHttpServletResponse();
		MockFilterChain end = new MockFilterChain();
		proxy.doFilter(request, response, end);
		return new Outcome(response, end.getRequest() != null);
	}

	private record Outcome(MockHttpServletResponse response, boolean reachedTheApplication) {
		// what a request met in the chain
	}
}
