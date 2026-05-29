package org.skyve.impl.web.spring;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * Registers top-level Spring configuration for the Skyve WAR runtime.
 *
 * <p>This class intentionally serves as a lightweight import anchor.
 */
@Configuration
@Import(SpringSecurityConfig.class)
public class SpringConfig {
	// nothing to see here
}
