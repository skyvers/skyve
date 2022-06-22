package org.skyve.impl.web.spring;

import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationEventPublisher;
import org.springframework.security.authentication.DefaultAuthenticationEventPublisher;

@Configuration
public class SkyveSpringSecurityConfig {
	@Bean
	@SuppressWarnings("static-method")
	public SkyveSpringSecurity skyveSpringSecurity() {
		return new SkyveSpringSecurity();
	}

	/**
	 * Required for the SecurityListener class to be registered.
	 */
	@Bean
	@SuppressWarnings("static-method")
	public AuthenticationEventPublisher authenticationEventPublisher(ApplicationEventPublisher applicationEventPublisher) {
		return new DefaultAuthenticationEventPublisher(applicationEventPublisher);
	}
}
