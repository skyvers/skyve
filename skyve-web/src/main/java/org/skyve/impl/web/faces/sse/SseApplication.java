package org.skyve.impl.web.faces.sse;

import java.util.Set;

import jakarta.ws.rs.ApplicationPath;
import jakarta.ws.rs.core.Application;

@ApplicationPath("/sse")
public class SseApplication extends Application {

	@Override
	public Set<Class<?>> getClasses() {
		return Set.of(SseClientHandler.class);
	}

}
