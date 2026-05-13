package org.skyve.impl.web.faces.sse;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.servlet.ServletContainer;
import org.glassfish.jersey.servlet.ServletProperties;
import org.glassfish.jersey.test.DeploymentContext;
import org.glassfish.jersey.test.JerseyTest;
import org.glassfish.jersey.test.ServletDeploymentContext;
import org.glassfish.jersey.test.grizzly.GrizzlyWebTestContainerFactory;
import org.glassfish.jersey.test.spi.TestContainerFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.web.service.sse.SseClientHandler;
import org.skyve.util.PushMessage;

import jakarta.ws.rs.core.Response;

/**
 * Tests SSE endpoint behaviour when no authenticated user is present in the
 * HTTP session.  A separate test class is necessary because the deployment
 * context must be configured <em>without</em> the user-injection servlet
 * filter used by {@link SseClientHandlerTest}.
 */
class SseClientHandlerNoUserTest extends JerseyTest {

	/**
	 * Resets {@link PushMessage#RECEIVERS} and stops any running reaper before
	 * each test to ensure a clean baseline.
	 */
	@BeforeEach
	void clearReceivers() {
		PushMessage.stopReaper();
		PushMessage.RECEIVERS.clear();
	}

	/**
	 * Defensive teardown that stops the reaper and clears receivers after each
	 * test in case an assertion failure left the collection dirty.
	 */
	@AfterEach
	void cleanupReceivers() {
		PushMessage.stopReaper();
		PushMessage.RECEIVERS.clear();
	}

	/**
	 * Configures the Jersey test deployment with {@link SseClientHandler} only —
	 * no session-injection filter.  Requests therefore arrive without an
	 * authenticated user in the HTTP session.
	 *
	 * @return the deployment context for the embedded Grizzly servlet container
	 */
	@Override
	protected DeploymentContext configureDeployment() {
		ResourceConfig config = new ResourceConfig(SseClientHandler.class);
		config.property(ServletProperties.JAXRS_APPLICATION_CLASS, ResourceConfig.class.getName());
		// No session-injection filter — requests arrive without an authenticated user.
		return ServletDeploymentContext
				.forServlet(new ServletContainer(config))
				.build();
	}

	/**
	 * Returns a Grizzly-based servlet test container factory so that the full
	 * servlet pipeline (session management) is exercised.
	 *
	 * @return the {@link GrizzlyWebTestContainerFactory}
	 */
	@Override
	protected TestContainerFactory getTestContainerFactory() {
		return new GrizzlyWebTestContainerFactory();
	}

	/**
	 * Verifies that a request to the SSE stream endpoint that carries no
	 * authenticated user in the session is rejected early: the handler must
	 * not register itself in {@link PushMessage#RECEIVERS}.
	 */
	@Test
	void testUnauthenticatedRequestDoesNotRegisterReceiver() {
		assertThat(PushMessage.RECEIVERS.size(), is(0));

		Response response = target("stream").request().get();
		response.close();

		assertThat("Unauthenticated request must not register a push receiver",
				PushMessage.RECEIVERS.size(), is(0));
	}
}
