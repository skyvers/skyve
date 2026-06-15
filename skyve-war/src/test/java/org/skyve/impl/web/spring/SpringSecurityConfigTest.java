package org.skyve.impl.web.spring; // NOSONAR java:S1220

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.provisioning.UserDetailsManager;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

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
}
