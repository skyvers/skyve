package org.skyve.impl.web.service.rest;

import java.util.HashSet;
import java.util.Set;

import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Application;

import org.skyve.impl.content.rest.RestRemoteContentManagerServer;

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
