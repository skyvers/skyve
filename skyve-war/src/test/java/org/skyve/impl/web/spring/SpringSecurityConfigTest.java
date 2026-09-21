package org.skyve.impl.web.spring; // NOSONAR java:S1220

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.security.authentication.AuthenticationDetailsSource;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.ObjectPostProcessor;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.provisioning.UserDetailsManager;
import org.springframework.security.web.DefaultSecurityFilterChain;
import org.springframework.security.web.authentication.AbstractAuthenticationProcessingFilter;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.authentication.WebAuthenticationDetails;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

import jakarta.servlet.Filter;
import jakarta.servlet.http.HttpServletRequest;

class SpringSecurityConfigTest {
	private SpringSecurityConfig config;
	private SkyveSpringSecurity skyve;

	@BeforeEach
	void setUp() {
		skyve = mock(SkyveSpringSecurity.class);
		config = new SpringSecurityConfig(skyve);
	}

	@Test
	void passwordEncoderDelegatesToSkyveSpringSecurity() {
		PasswordEncoder expected = mock(PasswordEncoder.class);
		when(skyve.passwordEncoder()).thenReturn(expected);

		assertSame(expected, config.passwordEncoder());
	}

	@Test
	void tokenRepositoryDelegatesToSkyveSpringSecurity() {
		PersistentTokenRepository expected = mock(PersistentTokenRepository.class);
		when(skyve.tokenRepository()).thenReturn(expected);

		assertSame(expected, config.tokenRepository());
	}

	@Test
	void userDetailsManagerDelegatesToSkyveSpringSecurity() {
		UserDetailsManager expected = mock(UserDetailsManager.class);
		when(skyve.jdbcUserDetailsManager()).thenReturn(expected);

		assertSame(expected, config.userDetailsManager());
	}

	@Test
	void clientRegistrationRepositoryDelegatesToSkyveSpringSecurity() {
		ClientRegistrationRepository expected = mock(ClientRegistrationRepository.class);
		when(skyve.clientRegistrationRepository()).thenReturn(expected);

		assertSame(expected, config.clientRegistrationRepository());
	}

	@Test
	void filterChainUsesSameAuthenticationDetailsSourceForFormAndTwoFactorLogin() throws Exception {
		AuthenticationDetailsSource<HttpServletRequest, WebAuthenticationDetails> detailsSource = mock(
				AuthenticationDetailsSource.class);
		UserDetailsManager userDetailsManager = mock(UserDetailsManager.class);
		AuthenticationManager authenticationManager = mock(AuthenticationManager.class);
		PersistentTokenRepository tokenRepository = mock(PersistentTokenRepository.class);

		when(skyve.authenticationDetailsSource()).thenReturn(detailsSource);
		when(skyve.jdbcUserDetailsManager()).thenReturn(userDetailsManager);
		when(skyve.tokenRepository()).thenReturn(tokenRepository);

		ObjectPostProcessor<Object> objectPostProcessor = new ObjectPostProcessor<>() {
			@Override
			public <O extends Object> O postProcess(O object) {
				return object;
			}
		};
		AuthenticationManagerBuilder authenticationBuilder = new AuthenticationManagerBuilder(objectPostProcessor);
		Map<Class<?>, Object> sharedObjects = new HashMap<>();
		String originalServerUrl = UtilImpl.SERVER_URL;
		String originalContext = UtilImpl.SKYVE_CONTEXT;
		String originalHomeUri = UtilImpl.HOME_URI;
		try (GenericApplicationContext applicationContext = new GenericApplicationContext()) {
			UtilImpl.SERVER_URL = "";
			UtilImpl.SKYVE_CONTEXT = "";
			UtilImpl.HOME_URI = "/home";
			applicationContext.refresh();
			sharedObjects.put(ApplicationContext.class, applicationContext);
			sharedObjects.put(UserDetailsService.class, userDetailsManager);
			HttpSecurity http = new HttpSecurity(objectPostProcessor, authenticationBuilder, sharedObjects);
			http.authenticationManager(authenticationManager);

			DefaultSecurityFilterChain chain = (DefaultSecurityFilterChain) config.filterChain(http);
			assertSame(detailsSource, authenticationDetailsSource(findFilter(chain, UsernamePasswordAuthenticationFilter.class)));
			assertSame(detailsSource, authenticationDetailsSource(findFilter(chain, TwoFactorAuthPushEmailFilter.class)));
		} finally {
			UtilImpl.SERVER_URL = originalServerUrl;
			UtilImpl.SKYVE_CONTEXT = originalContext;
			UtilImpl.HOME_URI = originalHomeUri;
		}

		verify(skyve).authenticationDetailsSource();
	}

	private static Filter findFilter(DefaultSecurityFilterChain chain, Class<? extends Filter> filterType) {
		return chain.getFilters()
				.stream()
				.filter(filterType::isInstance)
				.findFirst()
				.orElseThrow();
	}

	private static Object authenticationDetailsSource(Filter filter) throws ReflectiveOperationException {
		Field field = AbstractAuthenticationProcessingFilter.class.getDeclaredField("authenticationDetailsSource");
		field.setAccessible(true);
		return field.get(filter);
	}
}
