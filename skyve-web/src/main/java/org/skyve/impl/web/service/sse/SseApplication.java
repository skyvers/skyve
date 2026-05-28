package org.skyve.impl.web.service.sse;

import java.util.Set;

import jakarta.ws.rs.ApplicationPath;
import jakarta.ws.rs.core.Application;

/**
 * Implements internal web-module behavior for this Skyve runtime concern.
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
