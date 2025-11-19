package org.skyve.impl.web.faces.sse;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.servlet.ServletContainer;
import org.glassfish.jersey.servlet.ServletProperties;
import org.glassfish.jersey.test.DeploymentContext;
import org.glassfish.jersey.test.JerseyTest;
import org.glassfish.jersey.test.ServletDeploymentContext;
import org.glassfish.jersey.test.grizzly.GrizzlyWebTestContainerFactory;
import org.glassfish.jersey.test.spi.TestContainerFactory;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.util.PushMessage;
import org.skyve.web.WebContext;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.client.WebTarget;
import jakarta.ws.rs.sse.InboundSseEvent;
import jakarta.ws.rs.sse.SseEventSource;

public class SseClientHandlerTest extends JerseyTest {

	private static final String TEST_USER_ID = "dead-beef";

	@Override
	protected DeploymentContext configureDeployment() {
		ResourceConfig config = new ResourceConfig(SseClientHandler.class);

		config.property(ServletProperties.JAXRS_APPLICATION_CLASS, ResourceConfig.class.getName());
		return ServletDeploymentContext
				.forServlet(new ServletContainer(config))
				.addFilter(SessionInjectionServletFilter.class, "testfilter")
				.build();
	}

	@Override
	protected TestContainerFactory getTestContainerFactory() {

		// We need a servlet based container
		return new GrizzlyWebTestContainerFactory();
	}

	@Test
	public void testStreamEventsEndpoint() throws Exception {

		assertThat(PushMessage.RECEIVERS.size(), is(0));

		WebTarget target = target("stream");

		List<InboundSseEvent> events = new ArrayList<>();

		try (SseEventSource eventSource = SseEventSource.target(target).build()) {
			eventSource.register(
					inboundEvent -> {
						events.add(inboundEvent);
					},
					ex -> {
						// handle error
						System.out.print(ex);
					},
					() -> {
						// on complete
					});
			eventSource.open();

			PushMessage msg = new PushMessage()
					.user(TEST_USER_ID)
					.growl(MessageSeverity.info, "I'm a unit test");
			EXT.push(msg);

			// Wait one moment
			TimeUnit.SECONDS.sleep(1);

			// Close the client side
			eventSource.close();

			// Spam a few extra messages so the SseClientHandler detects the disconnect quicker
			for (int surplusMsgCount = 16; surplusMsgCount > 0; --surplusMsgCount) {
				EXT.push(msg);
			}

			// Wait for a bit to let SseClientHandler detect the disconnect and clean-up
			for (int waitIterations = 60; PushMessage.RECEIVERS.size() > 0 && waitIterations > 0; --waitIterations) {
				TimeUnit.SECONDS.sleep(1);
			}
		}

		// Should have received at least one event
		assertThat(events.size(), greaterThan(1));

		// SseClientHandler should have unregistered itself
		assertThat(PushMessage.RECEIVERS.size(), is(0));
	}

	public static class SessionInjectionServletFilter implements Filter {

		@Override
		public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
				throws IOException, ServletException {

			UserImpl u = new UserImpl();
			u.setId(TEST_USER_ID);
			u.setName("test-user");

			HttpServletRequest hsr = (HttpServletRequest) request;
			hsr.getSession().setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, u);

			chain.doFilter(request, response);
		}

	}

}
