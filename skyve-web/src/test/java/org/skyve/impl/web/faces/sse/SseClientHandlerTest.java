package org.skyve.impl.web.faces.sse;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.TimeUnit;

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
import org.skyve.EXT;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.service.sse.SseClientHandler;
import org.skyve.web.WebContext;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.client.Client;
import jakarta.ws.rs.client.ClientBuilder;
import jakarta.ws.rs.client.ClientRequestFilter;
import jakarta.ws.rs.client.WebTarget;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.sse.InboundSseEvent;
import jakarta.ws.rs.sse.OutboundSseEvent;
import jakarta.ws.rs.sse.SseEventSink;
import jakarta.ws.rs.sse.SseEventSource;

/**
 * Integration and unit tests for {@link SseClientHandler}.
 *
 * <p>Integration tests (those that require a live HTTP connection) extend
 * {@link JerseyTest} and are run against an embedded Grizzly servlet container.
 * A {@link SessionInjectionServletFilter} is registered in the deployment so
 * that every inbound request carries an authenticated {@link org.skyve.metadata.user.User}
 * in the HTTP session, mirroring normal application behaviour.
 *
 * <p>Unit tests operate directly on a {@link SseClientHandler} instance via
 * package-visible reflection helpers ({@link #getMessageQueue},
 * {@link #setSink}, {@link #setUserId}) so that the JAX-RS machinery is not
 * required and the tests remain fast and deterministic.
 *
 * <p><strong>Static state:</strong> {@link PushMessage#RECEIVERS} and the
 * push reaper are cleaned up in {@link #clearReceiversBeforeTest()} /
 * {@link #clearReceiversAfterTest()} so that tests do not interfere with each
 * other.
 */
@SuppressWarnings({"resource", "boxing", "java:S4144", "java:S1612", "java:S2925"})
class SseClientHandlerTest extends JerseyTest {

	private static final String TEST_USER_ID = "dead-beef";

	/**
	 * Ensure RECEIVERS is empty and no reaper is running before each test so
	 * that static state from one test cannot affect another.
	 */
	@BeforeEach
	@SuppressWarnings("static-method")
	void clearReceiversBeforeTest() {
		org.skyve.util.PushMessage.stopReaper();
		org.skyve.util.PushMessage.RECEIVERS.clear();
	}

	/**
	 * Defensive teardown: stop the reaper and clear RECEIVERS after each test
	 * in case a test threw before reaching its own finally block.
	 */
	@AfterEach
	@SuppressWarnings("static-method")
	void clearReceiversAfterTest() {
		org.skyve.util.PushMessage.stopReaper();
		org.skyve.util.PushMessage.RECEIVERS.clear();
	}

	/**
	 * Configures the Jersey test deployment with {@link SseClientHandler} and the
	 * {@link SessionInjectionServletFilter} that injects a test user into every
	 * HTTP session, matching the security context expected by the endpoint.
	 *
	 * @return the deployment context for the embedded Grizzly servlet container
	 */
	@Override
	protected DeploymentContext configureDeployment() {
		ResourceConfig config = new ResourceConfig(SseClientHandler.class);

		config.property(ServletProperties.JAXRS_APPLICATION_CLASS, ResourceConfig.class.getName());
		return ServletDeploymentContext
				.forServlet(new ServletContainer(config))
				.addFilter(SessionInjectionServletFilter.class, "testfilter")
				.build();
	}

	/**
	 * Returns a Grizzly-based servlet test container factory so that the
	 * full servlet pipeline (filters, session management) is exercised.
	 *
	 * @return the {@link GrizzlyWebTestContainerFactory}
	 */
	@Override
	protected TestContainerFactory getTestContainerFactory() {
		return new GrizzlyWebTestContainerFactory();
	}

	@Test
	void testStreamEventsEndpoint() throws Exception {
		int originalKeepAlive = UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS;
		UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS = 1;
		try {

			assertEquals(0, org.skyve.util.PushMessage.RECEIVERS.size());

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

				org.skyve.util.PushMessage msg = new org.skyve.util.PushMessage()
						.user(TEST_USER_ID)
						.growl(MessageSeverity.info, "I'm a unit test");
				EXT.push(msg);

				// Wait one moment
				TimeUnit.SECONDS.sleep(1);

				// Close the client side
				eventSource.close();

				// Wait for a keep-alive attempt to detect the disconnect and clean-up
				for (int waitIterations = 5; org.skyve.util.PushMessage.RECEIVERS.size() > 0 && waitIterations > 0; --waitIterations) {
					TimeUnit.SECONDS.sleep(1);
				}
			}

			// Should have received at least one event
			assertThat(events.size(), greaterThan(1));

			// Data events should carry an event ID for Last-Event-ID reconnection support
			InboundSseEvent dataEvent = events.stream()
					.filter(e -> e.readData() != null && !e.readData().isEmpty())
					.findFirst()
					.orElse(null);
			assertThat("Expected at least one data event", dataEvent, is(not(nullValue())));
			assertThat("Data events should have an ID", dataEvent.getId(), is(not(nullValue())));

			// SseClientHandler should have unregistered itself
			assertEquals(0, org.skyve.util.PushMessage.RECEIVERS.size());
		} finally {
			UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS = originalKeepAlive;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testCloseCleansUpHandlerState() throws Exception {
		SseClientHandler handler = new SseClientHandler();
		TrackingSseEventSink sink = new TrackingSseEventSink();

		setSink(handler, sink);
		org.skyve.util.PushMessage.RECEIVERS.add(handler);
		getMessageQueue(handler).offerLast(new org.skyve.util.PushMessage().growl(MessageSeverity.info, "queued"));

		assertTrue(org.skyve.util.PushMessage.RECEIVERS.contains(handler));
		assertFalse(getMessageQueue(handler).isEmpty());

		handler.close();

		assertFalse(org.skyve.util.PushMessage.RECEIVERS.contains(handler), "close() should deregister the receiver");
		assertTrue(getMessageQueue(handler).isEmpty(), "close() should clear queued messages");
		assertTrue(sink.isClosed(), "close() should close the sink");

		// Ensure idempotency
		handler.close();
		assertFalse(org.skyve.util.PushMessage.RECEIVERS.contains(handler));
	}

	@Test
	@SuppressWarnings("static-method")
	void testSendMessageIgnoredAfterClose() throws Exception {
		SseClientHandler handler = new SseClientHandler();
		TrackingSseEventSink sink = new TrackingSseEventSink();

		setSink(handler, sink);
		handler.close();

		handler.sendMessage(new org.skyve.util.PushMessage().growl(MessageSeverity.info, "after-close"));

		assertTrue(getMessageQueue(handler).isEmpty(), "sendMessage() should be ignored after close");
	}

	@Test
	void testStreamRejectedWhenGlobalReceiverLimitReached() {
		int originalMaxTotal = UtilImpl.PUSH_MAX_RECEIVERS_TOTAL;
		try {
			UtilImpl.PUSH_MAX_RECEIVERS_TOTAL = 1;

			org.skyve.util.PushMessage.PushMessageReceiver existing = new org.skyve.util.PushMessage.PushMessageReceiver() {
				@Override
				public String forUserId() {
					return "existing-user";
				}

				@Override
				public void sendMessage(org.skyve.util.PushMessage message) {
					// no-op
				}
			};
			org.skyve.util.PushMessage.RECEIVERS.add(existing);

			assertEquals(1, org.skyve.util.PushMessage.RECEIVERS.size());

			Response response = target("stream").request().get();
			response.close();

			assertEquals(1, org.skyve.util.PushMessage.RECEIVERS.size(), "global cap should prevent a new SSE receiver registration");
			assertTrue(org.skyve.util.PushMessage.RECEIVERS.contains(existing));
		} finally {
			org.skyve.util.PushMessage.RECEIVERS.clear();
			UtilImpl.PUSH_MAX_RECEIVERS_TOTAL = originalMaxTotal;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testMessageQueueDropsOldestWhenFull() throws Exception {
		int originalQueueSize = UtilImpl.PUSH_MESSAGE_QUEUE_SIZE;
		UtilImpl.PUSH_MESSAGE_QUEUE_SIZE = 2;
		try {
			SseClientHandler handler = new SseClientHandler();
			TrackingSseEventSink sink = new TrackingSseEventSink();
			setSink(handler, sink);

			org.skyve.util.PushMessage first = new org.skyve.util.PushMessage().growl(MessageSeverity.info, "first");
			org.skyve.util.PushMessage second = new org.skyve.util.PushMessage().growl(MessageSeverity.info, "second");
			org.skyve.util.PushMessage third = new org.skyve.util.PushMessage().growl(MessageSeverity.info, "third");

			handler.sendMessage(first);
			handler.sendMessage(second);
			handler.sendMessage(third);

			assertThat(new ArrayList<>(getMessageQueue(handler)), contains(second, third));
		} finally {
			UtilImpl.PUSH_MESSAGE_QUEUE_SIZE = originalQueueSize;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testIsStaleReturnsTrueAfterTimeout() throws Exception {
		int originalTimeout = UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS;
		try {
			// Set a very short stale timeout (1 second)
			UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS = 1;

			SseClientHandler handler = new SseClientHandler();

			// Should not be stale immediately
			assertFalse(handler.isStale());

			// Wait for the timeout to elapse
			TimeUnit.MILLISECONDS.sleep(1100);

			// Should now be stale
			assertTrue(handler.isStale());
		} finally {
			UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS = originalTimeout;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testIsStaleReturnsFalseWhenDisabled() {
		int originalTimeout = UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS;
		try {
			UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS = 0;

			SseClientHandler handler = new SseClientHandler();
			assertFalse(handler.isStale());
		} finally {
			UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS = originalTimeout;
		}
	}

	/**
	 * Verifies that opening a new SSE connection for a user that is already at
	 * the per-user handler limit evicts the oldest handler before registering
	 * the new one, so the per-user count never exceeds {@code maxReceiversPerUser}.
	 */
	@Test
	void testPerUserEvictionEvictsOldestHandler() throws Exception {
		int originalMaxPerUser = UtilImpl.PUSH_MAX_RECEIVERS_PER_USER;
		int originalKeepAlive = UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS;
		UtilImpl.PUSH_MAX_RECEIVERS_PER_USER = 1;
		UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS = 1;
		try {
			// Simulate an existing SSE handler for the test user
			SseClientHandler existingHandler = new SseClientHandler();
			setUserId(existingHandler, TEST_USER_ID);
			TrackingSseEventSink existingSink = new TrackingSseEventSink();
			setSink(existingHandler, existingSink);
			org.skyve.util.PushMessage.RECEIVERS.add(existingHandler);
			assertEquals(1, org.skyve.util.PushMessage.RECEIVERS.size());

			// Open a new SSE connection for the same user – eviction must fire
			try (SseEventSource eventSource = SseEventSource.target(target("stream")).build()) {
				eventSource.open();

				// Allow the server thread time to process the request and run evictExcessHandlers()
				TimeUnit.MILLISECONDS.sleep(500);

				assertFalse(org.skyve.util.PushMessage.RECEIVERS.contains(existingHandler), "Old handler should be evicted");
				assertTrue(existingSink.isClosed(), "Evicted sink should be closed");
				assertEquals(1, org.skyve.util.PushMessage.RECEIVERS.size(), "New handler should be registered in its place");
			}

			// After the client closes, wait for the new handler to deregister
			for (int i = 5; org.skyve.util.PushMessage.RECEIVERS.size() > 0 && i > 0; i--) {
				TimeUnit.SECONDS.sleep(1);
			}
			assertEquals(0, org.skyve.util.PushMessage.RECEIVERS.size(), "Handler should deregister after client closes");
		} finally {
			org.skyve.util.PushMessage.RECEIVERS.clear();
			UtilImpl.PUSH_MAX_RECEIVERS_PER_USER = originalMaxPerUser;
			UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS = originalKeepAlive;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testReaperRemovesStaleReceivers() throws Exception {
		int originalTimeout = UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS;
		try {
			UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS = 1;

			SseClientHandler handler = new SseClientHandler();
			org.skyve.util.PushMessage.RECEIVERS.add(handler);

			assertEquals(1, org.skyve.util.PushMessage.RECEIVERS.size());

			// Start the reaper with a 1-second interval
			org.skyve.util.PushMessage.startReaper(1);

			// Wait for the stale timeout + reaper interval to elapse
			TimeUnit.MILLISECONDS.sleep(2500);

			// Reaper should have removed the stale handler
			assertEquals(0, org.skyve.util.PushMessage.RECEIVERS.size());
		} finally {
			org.skyve.util.PushMessage.stopReaper();
			org.skyve.util.PushMessage.RECEIVERS.clear();
			UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS = originalTimeout;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testPushSkipsStaleReceivers() throws Exception {
		int originalTimeout = UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS;
		try {
			UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS = 1;

			SseClientHandler handler = new SseClientHandler();
			org.skyve.util.PushMessage.RECEIVERS.add(handler);

			// Wait for handler to become stale
			TimeUnit.MILLISECONDS.sleep(1100);

			assertTrue(handler.isStale());

			// Push a broadcast — should not throw or deliver to the stale handler
			org.skyve.util.PushMessage msg = new org.skyve.util.PushMessage().growl(MessageSeverity.info, "broadcast");
			EXT.push(msg);

			// The handler's queue should be empty because push skipped it
			assertEquals(0, getMessageQueue(handler).size());
		} finally {
			org.skyve.util.PushMessage.RECEIVERS.clear();
			UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS = originalTimeout;
		}
	}

	/**
	 * Verifies that {@link SseClientHandler#sendMessage} is a no-op when the sink
	 * reference has never been set (i.e. the handler was constructed but no HTTP
	 * connection has been established yet).
	 */
	@Test
	@SuppressWarnings("static-method")
	void testSendMessageWithNullSink() throws Exception {
		SseClientHandler handler = new SseClientHandler();
		// sinkRef is null by default — never assigned

		handler.sendMessage(new org.skyve.util.PushMessage().growl(MessageSeverity.info, "test"));

		assertTrue(getMessageQueue(handler).isEmpty(), "sendMessage() should be ignored when sinkRef is null");
	}

	/**
	 * Verifies that {@link SseClientHandler#sendMessage} is a no-op when the
	 * underlying sink has already been closed (e.g. client disconnected).
	 */
	@Test
	@SuppressWarnings("static-method")
	void testSendMessageWithClosedSink() throws Exception {
		SseClientHandler handler = new SseClientHandler();
		TrackingSseEventSink sink = new TrackingSseEventSink();
		setSink(handler, sink);
		sink.close();

		handler.sendMessage(new org.skyve.util.PushMessage().growl(MessageSeverity.info, "test"));

		assertTrue(getMessageQueue(handler).isEmpty(), "sendMessage() should be ignored when sink is closed");
	}

	/**
	 * Verifies that {@link SseClientHandler#forUserId()} returns {@code null}
	 * before a real HTTP request has been processed (i.e. the userId field has
	 * not yet been populated by {@code streamEvents}).
	 */
	@Test
	@SuppressWarnings("static-method")
	void testForUserIdReturnsNullBeforeConnection() {
		SseClientHandler handler = new SseClientHandler();
		assertThat("forUserId() should return null before any connection is established",
				handler.forUserId(), is(nullValue()));
	}

	/**
	 * Smoke-tests {@link SseClientHandler#toString()} to ensure it does not
	 * throw and returns a non-null, non-empty string even on a freshly
	 * constructed handler whose fields have not yet been initialised.
	 */
	@Test
	@SuppressWarnings("static-method")
	void testToStringReturnsNonNull() {
		SseClientHandler handler = new SseClientHandler();
		String str = handler.toString();
		assertThat("toString() should return a non-null string", str, is(not(nullValue())));
		assertFalse(str.isEmpty(), "toString() should not be empty");
	}

	/**
	 * Verifies that messages placed onto the queue appear in FIFO order when
	 * the queue is not full.
	 */
	@Test
	@SuppressWarnings("static-method")
	void testMessageQueueFIFOOrdering() throws Exception {
		SseClientHandler handler = new SseClientHandler();
		TrackingSseEventSink sink = new TrackingSseEventSink();
		setSink(handler, sink);

		org.skyve.util.PushMessage first = new org.skyve.util.PushMessage().growl(MessageSeverity.info, "first");
		org.skyve.util.PushMessage second = new org.skyve.util.PushMessage().growl(MessageSeverity.info, "second");

		handler.sendMessage(first);
		handler.sendMessage(second);

		assertThat(new ArrayList<>(getMessageQueue(handler)), contains(first, second));
	}

	/**
	 * Verifies that a client reconnecting with a {@code Last-Event-ID} header
	 * causes subsequent event IDs to be greater than that value, confirming
	 * that {@code streamEvents} advances {@link SseClientHandler#eventIdSequence}
	 * past the supplied ID before sending new events.
	 */
	@Test
	void testLastEventIdReconnectAdvancesEventIdSequence() throws Exception {
		int originalKeepAlive = UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS;
		UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS = 1;
		final long RECONNECT_FROM_ID = 100L;
		Client client = ClientBuilder.newBuilder()
				.register((ClientRequestFilter) ctx ->
						ctx.getHeaders().add("Last-Event-ID", String.valueOf(RECONNECT_FROM_ID)))
				.build();
		try {
			WebTarget reconnectTarget = client.target(getBaseUri()).path("stream");
			List<InboundSseEvent> events = new ArrayList<>();

			try (SseEventSource eventSource = SseEventSource.target(reconnectTarget).build()) {
				eventSource.register(events::add);
				eventSource.open();

				EXT.push(new org.skyve.util.PushMessage().user(TEST_USER_ID).growl(MessageSeverity.info, "after-reconnect"));

				TimeUnit.SECONDS.sleep(2);
				eventSource.close();

				for (int i = 5; org.skyve.util.PushMessage.RECEIVERS.size() > 0 && i > 0; i--) {
					TimeUnit.SECONDS.sleep(1);
				}
			}

			InboundSseEvent dataEvent = events.stream()
					.filter(e -> e.readData() != null && !e.readData().isEmpty())
					.findFirst()
					.orElse(null);

			assertThat("Should receive a data event after reconnect with Last-Event-ID",
					dataEvent, is(not(nullValue())));
			long receivedId = Long.parseLong(dataEvent.getId());
			assertThat("Event ID should be greater than the supplied Last-Event-ID",
					receivedId, greaterThan(RECONNECT_FROM_ID));
		} finally {
			client.close();
			UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS = originalKeepAlive;
		}
	}

	/**
	 * Verifies that a non-numeric {@code Last-Event-ID} header is silently
	 * ignored and that the event-ID sequence therefore starts from 1 (as it
	 * would on a fresh connection with no header at all).
	 */
	@Test
	void testNonNumericLastEventIdIsIgnored() throws Exception {
		int originalKeepAlive = UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS;
		UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS = 1;
		Client client = ClientBuilder.newBuilder()
				.register((ClientRequestFilter) ctx ->
						ctx.getHeaders().add("Last-Event-ID", "not-a-number"))
				.build();
		try {
			WebTarget reconnectTarget = client.target(getBaseUri()).path("stream");
			List<InboundSseEvent> events = new ArrayList<>();

			try (SseEventSource eventSource = SseEventSource.target(reconnectTarget).build()) {
				eventSource.register(events::add);
				eventSource.open();

				EXT.push(new org.skyve.util.PushMessage().user(TEST_USER_ID).growl(MessageSeverity.info, "test"));

				TimeUnit.SECONDS.sleep(2);
				eventSource.close();

				for (int i = 5; org.skyve.util.PushMessage.RECEIVERS.size() > 0 && i > 0; i--) {
					TimeUnit.SECONDS.sleep(1);
				}
			}

			InboundSseEvent dataEvent = events.stream()
					.filter(e -> e.readData() != null && !e.readData().isEmpty())
					.findFirst()
					.orElse(null);

			assertThat("Should receive a data event even when Last-Event-ID is non-numeric",
					dataEvent, is(not(nullValue())));
			// A non-numeric Last-Event-ID is ignored: sequence starts at 0, so the
			// first incrementAndGet() produces 1.
			assertThat("Non-numeric Last-Event-ID should be ignored; event ID should start at 1",
					Long.parseLong(dataEvent.getId()), is(1L));
		} finally {
			client.close();
			UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS = originalKeepAlive;
		}
	}

	@SuppressWarnings("unchecked")
	private static BlockingDeque<org.skyve.util.PushMessage> getMessageQueue(SseClientHandler handler) throws Exception {
		Field field = SseClientHandler.class.getDeclaredField("messageQueue");
		field.setAccessible(true);
		return (BlockingDeque<org.skyve.util.PushMessage>) field.get(handler);
	}

	private static void setSink(SseClientHandler handler, SseEventSink sink) throws Exception {
		Field field = SseClientHandler.class.getDeclaredField("sinkRef");
		field.setAccessible(true);
		field.set(handler, sink);
	}

	private static void setUserId(SseClientHandler handler, String userId) throws Exception {
		Field field = SseClientHandler.class.getDeclaredField("userId");
		field.setAccessible(true);
		field.set(handler, userId);
	}

	/**
	 * A minimal {@link SseEventSink} stub used in unit tests that do not need a
	 * real HTTP connection.
	 *
	 * <p>{@link #send} always returns an already-completed future so callers
	 * never block. {@link #close} simply flips the {@link #isClosed()} flag so
	 * that tests can assert whether the handler attempted to close the sink.
	 */
	private static final class TrackingSseEventSink implements SseEventSink {
		private volatile boolean closed;

		@Override
		public boolean isClosed() {
			return closed;
		}

		@Override
		public java.util.concurrent.CompletionStage<?> send(OutboundSseEvent event) {
			return java.util.concurrent.CompletableFuture.completedFuture(null);
		}

		@Override
		public void close() {
			closed = true;
		}
	}

	/**
	 * Servlet filter that plants a synthetic {@link UserImpl} into the HTTP
	 * session before each request reaches the JAX-RS endpoint.
	 *
	 * <p>This simulates the Skyve security filter that normally populates
	 * {@link WebContext#USER_SESSION_ATTRIBUTE_NAME} after a successful login,
	 * allowing integration tests to exercise the authenticated code paths
	 * without a real login flow.
	 */
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
