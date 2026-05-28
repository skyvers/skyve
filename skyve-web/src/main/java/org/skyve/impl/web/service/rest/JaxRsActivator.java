package org.skyve.impl.web.service.rest;

import java.util.HashSet;
import java.util.Set;

import org.skyve.impl.content.rest.RestRemoteContentManagerServer;

import jakarta.ws.rs.ApplicationPath;
import jakarta.ws.rs.core.Application;

/**
 * Registers JAX-RS resources exposed under the Skyve REST application path.
 */
@ApplicationPath("/rest")
public class JaxRsActivator extends Application {
	/**
	 * Returns JAX-RS resource classes exposed by the Skyve REST application.
	 */
	@Override
	public Set<Class<?>> getClasses() {
		final Set<Class<?>> classes = new HashSet<>();
		classes.add(RestService.class);
		classes.add(RestRemoteContentManagerServer.class);
		return classes;
	}
}
