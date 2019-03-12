package org.skyve.impl.web;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.ImportResource;

@Configuration
@Import(SpringSecurityConfig.class)
@ImportResource("WEB-INF/spring/security.xml")
public class SpringConfig {
	// nothing to see here
}
