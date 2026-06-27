package org.skyve.jar;

import java.util.HashSet;
import java.util.Set;

import org.jboss.resteasy.core.ResteasyDeploymentImpl;
import org.jboss.resteasy.plugins.server.undertow.UndertowJaxrsServer;
import org.jboss.resteasy.spi.ResteasyDeployment;
import org.jboss.weld.environment.servlet.Listener;
import org.skyve.impl.web.SkyveContextListener;

import io.undertow.Handlers;
import io.undertow.Undertow;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.server.handlers.resource.ClassPathResourceManager;
import io.undertow.server.handlers.resource.ResourceManager;
import io.undertow.servlet.Servlets;
import io.undertow.servlet.api.DeploymentInfo;
import io.undertow.servlet.api.ServletInfo;
import jakarta.ws.rs.ApplicationPath;
import jakarta.ws.rs.core.Application;

/**
 * Configures the JAX-RS application and boots an embedded Undertow runtime for
 * standalone Skyve execution.
 *
 * <p>Threading: application bootstrap is performed by the caller of
 * {@link #main(String[])}. The returned JAX-RS class set from
 * {@link #getClasses()} is immutable by convention after construction.
 */
@ApplicationPath("/")
public class SkyveApplication extends Application {
	/**
	 * Returns the JAX-RS resource classes exposed by this application.
	 *
	 * <p>Currently this method registers {@link MyResource} as the root resource
	 * type.
	 *
	 * @return the concrete resource class set for RESTEasy deployment; never
	 *         {@code null}
	 */
	@Override
	public Set<Class<?>> getClasses() {
		Set<Class<?>> classes = new HashSet<>();
		classes.add(MyResource.class); // Resource implemented in the previous section
		return classes;
	}

	/**
	 * Starts the embedded Undertow server and deploys this JAX-RS application.
	 *
	 * <p>Side effects: initializes RESTEasy deployment metadata, registers CDI
	 * listeners and servlet mappings, opens an HTTP listener on localhost:8080,
	 * and blocks the current thread for a fixed interval.
	 *
	 * @param args command-line arguments; currently ignored
	 * @throws InterruptedException if the blocking sleep at the end of startup is
	 *         interrupted
	 */
	public static void main(String[] args) throws InterruptedException {
		UndertowJaxrsServer server = new UndertowJaxrsServer();

		ResteasyDeployment deployment = new ResteasyDeploymentImpl();
		deployment.setApplicationClass(SkyveApplication.class.getName());
		deployment.setInjectorFactoryClass("org.jboss.resteasy.cdi.CdiInjectorFactory"); // set CDI injector factory

		DeploymentInfo deploymentInfo = server.undertowDeployment(deployment, "/rest");
		deploymentInfo.setClassLoader(Application.class.getClassLoader());
		deploymentInfo.setDeploymentName("Skyve Application"); // set name of deployment
		deploymentInfo.setContextPath("/");
		
//		deploymentInfo.app
		deploymentInfo.addListener(Servlets.listener(Listener.class)); // add Weld listener to deployment
		ServletInfo info = Servlets.servlet("shite", MyServlet.class);
		info.addMapping("/shite");
		deploymentInfo.addServlet(info);
		
		ResourceManager resourceManager = new ClassPathResourceManager(SkyveContextListener.class.getClassLoader(), "META-INF/resources/");

		server.deploy(deploymentInfo);

		// access the server on http://localhost:8080, note that "localhost" should be
		// "0.0.0.0" if you wish for others in the network to connect.
		Undertow.Builder builder = Undertow.builder().addHttpListener(8080, "localhost");
		builder.setHandler(new HttpHandler() {
			@Override
			public void handleRequest(HttpServerExchange exchange) throws Exception {
				Handlers.resource(resourceManager);
			}
		});
		builder.build().start();
		Thread.sleep(60000);
	}
}
