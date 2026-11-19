package org.skyve.impl.web.spring;

import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationEventPublisher;
import org.springframework.security.authentication.DefaultAuthenticationEventPublisher;

import jakarta.annotation.Nonnull;

/**
 * Declares Spring beans that connect Spring Security eventing and helper services to Skyve web runtime behavior.
 */
@Configuration
public class SkyveSpringSecurityConfig {
	/**
	 * Exposes the primary Skyve Spring Security helper bean.
	 */
	@Bean
	@SuppressWarnings("static-method")
	public @Nonnull SkyveSpringSecurity skyveSpringSecurity() {
		return new SkyveSpringSecurity();
	}

	/**
	 * Required for the SecurityListener class to be registered.
	 */
	@Bean
	@SuppressWarnings("static-method")
	public @Nonnull AuthenticationEventPublisher authenticationEventPublisher(@Nonnull ApplicationEventPublisher applicationEventPublisher) {
		return new DefaultAuthenticationEventPublisher(applicationEventPublisher);
	}
}
