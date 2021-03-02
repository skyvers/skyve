package org.skyve.impl.web.spring;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SkyveSpringSecurityConfig {
	@Bean
	@SuppressWarnings("static-method")
	public SkyveSpringSecurity skyveSpringSecurity() {
		return new SkyveSpringSecurity();
	}
}
