package org.skyve.impl.web.service.sse;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.user.User;
import org.skyve.util.JSON;
import org.skyve.util.PushMessage;
import org.skyve.util.PushMessage.PushMessageReceiver;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.MoreObjects;

import jakarta.enterprise.context.RequestScoped;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.HeaderParam;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.sse.OutboundSseEvent;
import jakarta.ws.rs.sse.Sse;
import jakarta.ws.rs.sse.SseEventSink;

/**
 * JAX-RS SSE endpoint that streams push messages to a single connected browser client.
 *
 * <p>Each HTTP request to {@code GET /stream} creates one instance of this class (CDI
 * {@code @RequestScoped}). The instance registers itself in {@link PushMessage#RECEIVERS}
 * and then blocks in {@link #sendMessageLoop} until one of the following occurs:
 * <ul>
 *   <li>The client closes the connection ({@code SseEventSink.isClosed()}).</li>
 *   <li>A send operation times out or fails.</li>
 *   <li>The container interrupts the request thread during shutdown.</li>
 *   <li>The stale-receiver reaper calls {@link #close()} because no successful send
 *       has occurred within {@link UtilImpl#PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS}.</li>
 * </ul>
 *
 * <p><strong>Backpressure:</strong> inbound messages are placed onto a bounded
 * {@link LinkedBlockingDeque} (capacity {@link UtilImpl#PUSH_MESSAGE_QUEUE_SIZE}).
 * When the queue is full the oldest queued message is evicted to make room.
 *
 * <p><strong>Connection limits:</strong>
 * <ul>
 *   <li>Per-user: if {@link UtilImpl#PUSH_MAX_RECEIVERS_PER_USER} &gt; 0, the oldest
 *       handlers for that user are evicted before this one is registered.</li>
 *   <li>Global: if {@link UtilImpl#PUSH_MAX_RECEIVERS_TOTAL} &gt; 0, the request is
 *       rejected when the limit is already reached.</li>
 * </ul>
 *
 * <p><strong>Thread safety:</strong> {@link #sendMessage(PushMessage)} may be called
 * from any thread. {@link #close()} is idempotent and safe to call concurrently.
 */
@Path("/")
@RequestScoped
public class SseClientHandler implements PushMessageReceiver {
	private static final Logger LOGGER = LoggerFactory.getLogger(SseClientHandler.class);

	private final BlockingDeque<PushMessage> messageQueue = new LinkedBlockingDeque<>(UtilImpl.PUSH_MESSAGE_QUEUE_SIZE);

	/**
	 * Monotonically increasing event ID for this handler, used to support
	 * client reconnection via the SSE {@code Last-Event-ID} header.
	 */
	private final AtomicLong eventIdSequence = new AtomicLong(0);

	/**
	 * Epoch millis of the last successful send (message or keep-alive).
	 * Used by the stale-receiver reaper to detect zombie handlers.
	 * 
	 * Volatile because it is written by the streaming thread and read by the reaper thread.
	 */
	private volatile long lastSuccessfulSendMillis = System.currentTimeMillis();

	/**
	 * Epoch millis when this handler was created. Used for per-user eviction
	 * (oldest handler is evicted first).
	 */
	private final long createdAtMillis = System.currentTimeMillis();

	/**
	 * Volatile because it is assigned by the JAX-RS container thread in streamEvents()
	 * and read by the reaper thread in close() and by other request threads in evictExcessHandlers().
	 */
	 // This reference is effectively immutable after streamEvents() assigns it, and the small window before assignment 
	// is benign because the handler is not registered in RECEIVERS until after assignment
	@SuppressWarnings({"java:S3077", "resource"})
	private volatile SseEventSink sinkRef;
	private final AtomicBoolean closed = new AtomicBoolean(false);
	private String userId;
	private String userName;

	@Context
	private Sse sse;

	/**
	 * JAX-RS resource method that opens and maintains the SSE stream for one client.
	 *
	 * <p>The method blocks for the lifetime of the connection. It performs the following
	 * steps in order:
	 * <ol>
	 *   <li>Resolves the authenticated {@link User} from the HTTP session; rejects
	 *       unauthenticated requests immediately.</li>
	 *   <li>Honours the {@code Last-Event-ID} request header so that clients which
	 *       reconnect after a network interruption resume from the correct sequence
	 *       position.</li>
	 *   <li>Calls {@link #evictExcessHandlers()} to enforce the per-user connection
	 *       cap before registering.</li>
	 *   <li>Checks the global connection cap and rejects the request if it is
	 *       already reached.</li>
	 *   <li>Registers this handler in {@link PushMessage#RECEIVERS} and enters
	 *       {@link #sendMessageLoop}.</li>
	 *   <li>Calls {@link #cleanupAndClose()} in a {@code finally} block regardless
	 *       of how the loop exits.</li>
	 * </ol>
	 *
	 * @param eventSink   the JAX-RS sink used to write events to the client
	 * @param request     the current HTTP servlet request (used to read the session)
	 * @param lastEventId value of the {@code Last-Event-ID} header supplied by a
	 *                    reconnecting client, or {@code null} on a fresh connection
	 * @throws InterruptedException if the container interrupts the request thread
	 *                              during server shutdown
	 */
	@GET
	@Path("/stream")
	@Produces(MediaType.SERVER_SENT_EVENTS)
	public void streamEvents(@Context SseEventSink eventSink,
								@Context HttpServletRequest request,
								@HeaderParam("Last-Event-ID") String lastEventId)
			throws InterruptedException {

		try (SseEventSink sink = eventSink) {
			sinkRef = sink;

			User user = Optional.of(request)
					.map(HttpServletRequest::getSession)
					.map(s -> s.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME))
					.map(User.class::cast)
					.orElse(null);

			if (user == null) {
				LOGGER.warn("No user provided");
				return;
			}

			userId = user.getId();
			userName = user.getName();

			// If the client reconnected with a Last-Event-ID, advance our sequence past it
			// so subsequent event IDs are always increasing.
			if (lastEventId != null && !lastEventId.isEmpty()) {
				try {
					long lastId = Long.parseLong(lastEventId);
					eventIdSequence.set(lastId);
					LOGGER.debug("Client '{}' reconnected with Last-Event-ID={}", userName, Long.valueOf(lastId));
				} catch (@SuppressWarnings("unused") NumberFormatException e) {
					LOGGER.debug("Ignoring non-numeric Last-Event-ID '{}' from '{}'", lastEventId, userName);
				}
			}

			LOGGER.debug("Starting stream to '{}'", userName);

			// Evict excess handlers for this user before registering
			evictExcessHandlers();

			if (!underGlobalReceiverLimit()) {
				LOGGER.warn("Rejecting SSE stream for '{}' because global push receiver limit ({}) has been reached",
						userName,
						Integer.valueOf(UtilImpl.PUSH_MAX_RECEIVERS_TOTAL));
				return;
			}

			// Register this handler
			PushMessage.RECEIVERS.add(this);

			// Loop and send
			sendMessageLoop(sink);
		} finally {
			cleanupAndClose();
			LOGGER.debug("Closing stream to {}", userName);
		}

	}

	/**
	 * Enforces the per-user SSE connection cap ({@link UtilImpl#PUSH_MAX_RECEIVERS_PER_USER}).
	 *
	 * <p>If the cap is not configured (value &le; 0) this method is a no-op. Otherwise
	 * it collects all currently registered handlers whose {@link #forUserId()} equals
	 * this handler's user ID, sorts them oldest-first by {@link #createdAtMillis}, and
	 * closes the excess ones so that after this handler is registered the per-user
	 * count does not exceed the configured maximum.
	 *
	 * <p>Called from {@link #streamEvents} before the handler is added to
	 * {@link PushMessage#RECEIVERS}.
	 */
	private void evictExcessHandlers() {
		int maxPerUser = UtilImpl.PUSH_MAX_RECEIVERS_PER_USER;
		if (maxPerUser <= 0) {
			return;
		}

		// Collect all existing handlers for this user
		List<SseClientHandler> userHandlers = new ArrayList<>();
		for (PushMessageReceiver receiver : PushMessage.RECEIVERS) {
			if (receiver instanceof SseClientHandler handler && userId.equals(handler.forUserId())) {
				userHandlers.add(handler);
			}
		}

		// maxPerUser - 1 because we are about to add ourselves
		int excess = userHandlers.size() - (maxPerUser - 1);
		if (excess <= 0) {
			return;
		}

		// Sort by creation time ascending (oldest first)
		userHandlers.sort((a, b) -> Long.compare(a.createdAtMillis, b.createdAtMillis));

		for (int i = 0; i < excess && i < userHandlers.size(); i++) {
			SseClientHandler victim = userHandlers.get(i);
			PushMessage.RECEIVERS.remove(victim);
			try {
				victim.close();
			} catch (@SuppressWarnings("unused") Exception e) {
				// Best effort
			}
			LOGGER.info("Evicted oldest SSE handler for user '{}' (limit {} per user)", userName,
					Integer.valueOf(maxPerUser));
		}
	}

	/**
	 * Returns {@code true} if another receiver may be registered without exceeding the
	 * global connection cap ({@link UtilImpl#PUSH_MAX_RECEIVERS_TOTAL}).
	 *
	 * <p>When the cap is not configured (value &le; 0), connections are unlimited and
	 * this method always returns {@code true}.
	 *
	 * @return {@code true} if the current receiver count is below the configured maximum
	 */
	private static boolean underGlobalReceiverLimit() {
		int maxTotal = UtilImpl.PUSH_MAX_RECEIVERS_TOTAL;
		if (maxTotal <= 0) {
			return true;
		}
		return PushMessage.RECEIVERS.size() < maxTotal;
	}

	/**
	 * Blocks and streams events until the connection is terminated.
	 *
	 * <p>On entry a keep-alive comment is sent immediately to flush the HTTP response
	 * headers to the client. The loop then waits up to
	 * {@link UtilImpl#PUSH_KEEP_ALIVE_TIME_IN_SECONDS} seconds for a message to appear
	 * on {@link #messageQueue}. If the wait times out a keep-alive comment is sent
	 * instead so the connection is not silently dropped by intermediate proxies.
	 *
	 * <p>The loop exits when any of the following is true:
	 * <ul>
	 *   <li>{@code sink.isClosed()} — the client has disconnected.</li>
	 *   <li>{@link #closed} — {@link #cleanupAndClose()} has already been called
	 *       (e.g. by the reaper).</li>
	 *   <li>A send attempt fails, times out, or is interrupted.</li>
	 * </ul>
	 *
	 * @param sink the event sink to write to
	 * @throws InterruptedException if the container interrupts this thread
	 */
	@SuppressWarnings("resource") // The sink is closed by the container after this method returns, or by cleanupAndClose() on error
	private void sendMessageLoop(SseEventSink sink) throws InterruptedException {
		// Immediately send a keep-alive to flush the headers to the client
		if (!sendEvent(sink, createKeepAlive())) {
			return;
		}

		while (!sink.isClosed() && !closed.get()) {
			// Wait for a new PushMessage to be sent
			PushMessage msg = messageQueue.poll(UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS, TimeUnit.SECONDS);

			if (msg == null) {

				// Send a keep alive message to the client instead
				if (!sendEvent(sink, createKeepAlive())) {
					return;
				}
				continue;
			}

			LOGGER.trace("Sending message '{}' to {}", msg, userName);

			if (!sendEvent(sink, createEvent(msg))) {
				return;
			}
		}
	}

	/**
	 * Sends a single SSE event and blocks until the write completes or a timeout occurs.
	 *
	 * <p>On success, {@link #lastSuccessfulSendMillis} is updated so that the stale-receiver
	 * reaper does not erroneously evict an active connection.
	 *
	 * <p>On any failure (timeout, interruption, or execution exception) the sink is closed
	 * via {@link #closeSink} and {@code false} is returned so the caller can exit the
	 * send loop cleanly.
	 *
	 * @param sink  the event sink to write to
	 * @param event the event to send
	 * @return {@code true} if the send completed successfully; {@code false} otherwise
	 */
	private boolean sendEvent(SseEventSink sink, OutboundSseEvent event) {
		try {
			sink.send(event).toCompletableFuture().get(UtilImpl.PUSH_SEND_TIMEOUT_IN_SECONDS, TimeUnit.SECONDS);
			lastSuccessfulSendMillis = System.currentTimeMillis();
			return true;
		}
		catch (@SuppressWarnings("unused") TimeoutException e) {
			LOGGER.debug("Send to {} timed out after {}s – treating client as stale", userName,
							Integer.valueOf(UtilImpl.PUSH_SEND_TIMEOUT_IN_SECONDS));
			closeSink(sink);
			return false;
		}
		catch (@SuppressWarnings("unused") InterruptedException e) {
			LOGGER.debug("Send to {} was interrupted – closing stream", userName);
			Thread.currentThread().interrupt();
			closeSink(sink);
			return false;
		}
		catch (ExecutionException | RuntimeException e) {
			logSendFailure(e);
			closeSink(sink);
			return false;
		}
	}

	/**
	 * Builds a keep-alive SSE comment event.
	 *
	 * <p>SSE comment lines ({@code :keep-alive}) are not delivered to JavaScript
	 * {@code EventSource} listeners but do keep the TCP connection open through
	 * idle-connection timeouts enforced by load balancers and proxies.
	 *
	 * @return a comment-only {@link OutboundSseEvent}
	 */
	private OutboundSseEvent createKeepAlive() {
		return sse.newEventBuilder().comment("keep-alive").build();
	}

	/**
	 * Builds a data {@link OutboundSseEvent} from a {@link PushMessage}.
	 *
	 * <p>The message's items are serialised to a JSON array and sent as the event data.
	 * Each event is assigned a monotonically increasing ID from {@link #eventIdSequence}
	 * so that reconnecting clients can supply it in the {@code Last-Event-ID} header and
	 * resume from the correct position in the stream.
	 *
	 * @param msg the push message to serialise
	 * @return an {@link OutboundSseEvent} carrying the serialised JSON payload
	 */
	private OutboundSseEvent createEvent(PushMessage msg) {
		String msgString = JSON.marshall(msg.getItems());
		long eventId = eventIdSequence.incrementAndGet();

		return sse.newEventBuilder()
				.id(String.valueOf(eventId))
				.reconnectDelay(UtilImpl.PUSH_KEEP_ALIVE_TIME_IN_SECONDS * 1000L)
				.data(msgString)
				.mediaType(MediaType.APPLICATION_JSON_TYPE)
				.build();
	}

	/**
	 * Logs an exceptional completion of a {@link SseEventSink#send} call at DEBUG level.
	 *
	 * <p>The cause of an {@link java.util.concurrent.ExecutionException} is unwrapped
	 * before logging so the message reflects the root failure rather than the wrapper.
	 *
	 * @param throwable the exception thrown or produced by the send future
	 */
	private void logSendFailure(Throwable throwable) {
		Throwable cause = throwable.getCause();
		Throwable failure = (cause == null) ? throwable : cause;

		LOGGER.atDebug()
				.setMessage("Sending to {} failed: {}")
				.addArgument(userName)
				.addArgument(failure.getMessage())
				.log();
	}

	/**
	 * Attempts to close the given {@link SseEventSink}, swallowing any
	 * {@link RuntimeException} that may be thrown if the sink is already closed
	 * or the underlying connection has been reset.
	 *
	 * @param sink the sink to close
	 */
	private void closeSink(SseEventSink sink) {
		try {
			sink.close();
		}
		catch (RuntimeException e) {
			LOGGER.atTrace()
					.setMessage("Closing stream to {} failed: {}")
					.addArgument(userName)
					.addArgument(e.getMessage())
					.log();
		}
	}

	/**
	 * Releases all resources held by this handler and deregisters it from
	 * {@link PushMessage#RECEIVERS}.
	 *
	 * <p>This method is idempotent: the first caller wins via {@link AtomicBoolean}
	 * and subsequent invocations return immediately without performing any work.
	 * It is therefore safe to call from multiple threads — for example, from the
	 * streaming thread in {@link #streamEvents} and concurrently from the reaper
	 * thread via {@link #close()}.
	 *
	 * <p>Actions performed on the first call:
	 * <ol>
	 *   <li>Removes this handler from {@link PushMessage#RECEIVERS}.</li>
	 *   <li>Snapshots and nulls {@link #sinkRef} to release the reference.</li>
	 *   <li>Clears {@link #messageQueue} to release queued message objects.</li>
	 *   <li>Closes the sink if it is not already closed.</li>
	 * </ol>
	 */
	private void cleanupAndClose() {
		if (!closed.compareAndSet(false, true)) {
			return;
		}

		PushMessage.RECEIVERS.remove(this);
		SseEventSink sink = sinkRef;
		sinkRef = null;
		messageQueue.clear();

		if (sink != null && !sink.isClosed()) {
			closeSink(sink);
		}
	}

	@Override
	public String forUserId() {
		return userId;
	}

	@Override
	public void sendMessage(PushMessage message) {
		if (closed.get()) {
			return;
		}

		SseEventSink sink = sinkRef;
		if ((sink == null) || sink.isClosed()) {
			return;
		}

		// No external synchronization needed — LinkedBlockingDeque is internally thread-safe,
		// and there is only one handler per user so contention on this queue is inherently low.
		// The small race between pollFirst() and offerLast() is benign: the worst case is an
		// extra message is dropped under extreme contention, which is already an accepted
		// degraded-service outcome.

		// Fast path: queue has space
		if (messageQueue.offerLast(message)) {
			return;
		}

		// Queue reported full, drop the oldest message and retry
		PushMessage dropped = messageQueue.pollFirst();
		if (dropped == null) {
			// This should not normally happen: the queue reported full but nothing could be polled
			LOGGER.error("Unable to drop oldest queued push message for {} because the queue was unexpectedly empty while reported full (capacity = {})",
							(userName == null) ? "unknown-user" : userName,
							Integer.valueOf(UtilImpl.PUSH_MESSAGE_QUEUE_SIZE));
			if (! messageQueue.offerLast(message)) {
				LOGGER.error("Dropping push message for {} because the queue remains full and no message could be dropped (capacity = {})",
								(userName == null) ? "unknown-user" : userName,
								Integer.valueOf(UtilImpl.PUSH_MESSAGE_QUEUE_SIZE));
			}
			return;
		}

		if (messageQueue.offerLast(message)) {
			LOGGER.warn("Dropped oldest queued push message for {} because the queue is full ({})",
							(userName == null) ? "unknown-user" : userName,
							Integer.valueOf(UtilImpl.PUSH_MESSAGE_QUEUE_SIZE));
			return;
		}

		// At this point we have dropped one message but still cannot enqueue the new one: unexpected error
		LOGGER.error("Dropping push message for {} because the queue remained full even after dropping the oldest message (capacity = {})",
						(userName == null) ? "unknown-user" : userName,
						Integer.valueOf(UtilImpl.PUSH_MESSAGE_QUEUE_SIZE));
	}

	@Override
	public boolean isStale() {
		int timeoutSeconds = UtilImpl.PUSH_STALE_RECEIVER_TIMEOUT_IN_SECONDS;
		if (timeoutSeconds <= 0) {
			return false;
		}
		long elapsed = System.currentTimeMillis() - lastSuccessfulSendMillis;
		return elapsed > (timeoutSeconds * 1000L);
	}

	@Override
	public void close() {
		cleanupAndClose();
	}

	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
				.add("userId", String.valueOf(userId))
				.add("userName", String.valueOf(userName))
				.add("eventId", eventIdSequence.get())
				.add("createdAt", createdAtMillis)
				.toString();
	}
}
