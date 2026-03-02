package org.skyve.impl.web.service.sse;

import java.util.Optional;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.TimeUnit;

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
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.sse.OutboundSseEvent;
import jakarta.ws.rs.sse.Sse;
import jakarta.ws.rs.sse.SseEventSink;

@Path("/")
@RequestScoped
public class SseClientHandler implements PushMessageReceiver {
	private static final Logger LOGGER = LoggerFactory.getLogger(SseClientHandler.class);

	private final BlockingDeque<PushMessage> messageQueue = new LinkedBlockingDeque<>(UtilImpl.PUSH_MESSAGE_QUEUE_SIZE);

	private String userId;
	private String userName;

	@Context
	private Sse sse;

	@GET
	@Path("/stream")
	@Produces(MediaType.SERVER_SENT_EVENTS)
	public void streamEvents(@Context SseEventSink eventSink, @Context HttpServletRequest request)
			throws InterruptedException {

		try (SseEventSink sink = eventSink) {

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

			LOGGER.debug("Starting stream to '{}'", userName);

			// Register this handler
			PushMessage.RECEIVERS.add(this);

			// Loop and send
			sendMessageLoop(sink);
		} finally {

			// Unregister ourselves
			PushMessage.RECEIVERS.remove(this);
			LOGGER.debug("Closing stream to {}", userName);
		}

	}

	/**
	 * Loop until the client disconnects; checking for messages added to 'messageQueue'; or
	 * until the container shuts down, which should throw an InterruptedException when
	 * the queue is polled.
	 */
	private void sendMessageLoop(SseEventSink sink) throws InterruptedException {

		// Immediately send a keep-alive to flush the headers to the client
		if (!sendEvent(sink, createKeepAlive())) {
			return;
		}

		while (!sink.isClosed()) {

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

	private boolean sendEvent(SseEventSink sink, OutboundSseEvent event) {
		try {
			sink.send(event).toCompletableFuture().join();
			return true;
		} catch (RuntimeException e) {
			logSendFailure(e);
			closeSink(sink);
			return false;
		}
	}

	private OutboundSseEvent createKeepAlive() {
		return sse.newEventBuilder()
				.comment("keep-alive")
				.build();
	}

	/**
	 * Create an OutboundSseEvent from the provided PushMessage.
	 */
	private OutboundSseEvent createEvent(PushMessage msg) {

		String msgString = JSON.marshall(msg.getItems());

		return sse.newEventBuilder()
				.data(msgString)
				.mediaType(MediaType.APPLICATION_JSON_TYPE)
				.build();
	}

	/**
	 * Log exceptional completions of <code>SseEventSink.send()</code> calls.
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

	private void closeSink(SseEventSink sink) {
		try {
			sink.close();
		} catch (RuntimeException e) {
			LOGGER.atTrace()
					.setMessage("Closing stream to {} failed: {}")
					.addArgument(userName)
					.addArgument(e.getMessage())
					.log();
		}
	}

	@Override
	public String forUserId() {
		return userId;
	}

	@Override
	public void sendMessage(PushMessage message) {
		if (messageQueue.offerLast(message)) {
			return;
		}

		PushMessage dropped = messageQueue.pollFirst();
		if (dropped != null && messageQueue.offerLast(message)) {
			LOGGER.warn("Dropped oldest queued push message for {} because the queue is full ({})",
					(userName == null) ? "unknown-user" : userName,
					Integer.valueOf(UtilImpl.PUSH_MESSAGE_QUEUE_SIZE));
			return;
		}

		LOGGER.warn("Dropping push message for {} because the queue remains full ({})",
				(userName == null) ? "unknown-user" : userName,
				Integer.valueOf(UtilImpl.PUSH_MESSAGE_QUEUE_SIZE));
	}

	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
				.add("userId", userId)
				.add("userName", userName)
				.toString();
	}

}
