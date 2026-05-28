package org.skyve.impl.web.service.sse;

import java.util.Set;

import jakarta.ws.rs.ApplicationPath;
import jakarta.ws.rs.core.Application;

/**
 * Boots the JAX-RS application that hosts Skyve server-sent event endpoints.
 */
@ApplicationPath("/sse")
public class SseApplication extends Application {
	/**
	 * Returns SSE resource classes exposed by the Skyve SSE application.
	 */
	@Override
	public Set<Class<?>> getClasses() {
		return Set.of(SseClientHandler.class);
	}
}
