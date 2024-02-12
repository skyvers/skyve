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

@ApplicationPath("/")
public class SkyveApplication extends Application {
	@Override
	public Set<Class<?>> getClasses() {
		Set<Class<?>> classes = new HashSet<>();
		classes.add(MyResource.class); // Resource implemented in the previous section
		return classes;
	}

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
//		deploymentInfo.setResourceManager(resourceManager);
			
//        deploymentInfo.addInitParameter(null, null)

		server.deploy(deploymentInfo);

		// access the server on http://localhost:8080, note that "localhost" should be
		// "0.0.0.0" if you wish for others in the network to connect.
		Undertow.Builder builder = Undertow.builder().addHttpListener(8080, "localhost");
		builder.setHandler(new HttpHandler() {
			@Override
			public void handleRequest(HttpServerExchange exchange) throws Exception {
				Handlers.resource(resourceManager);
//				Handlers.resource(new PathResourceManager(Paths.get(System.getProperty("user.home")), 100)).setDirectoryListingEnabled(true);
			}
		});
		builder.build().start();
//		server.start(builder);
		Thread.sleep(60000);
	}
}
