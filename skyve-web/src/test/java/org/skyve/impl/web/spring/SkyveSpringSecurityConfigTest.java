package org.skyve.impl.web.spring;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.security.authentication.AuthenticationEventPublisher;
import org.springframework.security.authentication.DefaultAuthenticationEventPublisher;

@SuppressWarnings("static-method")
class SkyveSpringSecurityConfigTest {
	@Test
	void createsConfiguredSecurityBeans() {
		SkyveSpringSecurityConfig config = new SkyveSpringSecurityConfig();
		SkyveSpringSecurity security = config.skyveSpringSecurity();
		assertNotNull(security);
		ApplicationEventPublisher publisher = event -> {
			// no-op publisher for bean construction verification
		};
		AuthenticationEventPublisher authenticationEventPublisher = config.authenticationEventPublisher(publisher);
		assertNotNull(authenticationEventPublisher);
		assertTrue(authenticationEventPublisher instanceof DefaultAuthenticationEventPublisher);
	}
}
