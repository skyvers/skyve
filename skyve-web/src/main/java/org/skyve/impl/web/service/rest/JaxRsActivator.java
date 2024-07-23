package org.skyve.impl.web.service.rest;

import java.util.HashSet;
import java.util.Set;

import org.skyve.impl.content.rest.RestRemoteContentManagerServer;

import jakarta.ws.rs.ApplicationPath;
import jakarta.ws.rs.core.Application;

@ApplicationPath("/rest")
public class JaxRsActivator extends Application {
	@Override
	public Set<Class<?>> getClasses() {
		final Set<Class<?>> classes = new HashSet<>();
		classes.add(RestService.class);
		classes.add(RestRemoteContentManagerServer.class);
		return classes;
	}
}
