package org.skyve.impl.web.faces.sse;

import java.util.Optional;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

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

	private static final int WAIT_SECONDS = 20;

	private static final Logger LOGGER = LoggerFactory.getLogger(SseClientHandler.class);

	private BlockingQueue<PushMessage> messageQueue = new LinkedBlockingQueue<>();

	private String userId;
	private String userName;

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
		}

		LOGGER.debug("Closed stream to {}", userName);
	}

	/**
	 * Loop until the client disconnects; checking for messages added to 'messageQueue'
	 */
	private void sendMessageLoop(SseEventSink sink) throws InterruptedException {

		while (!sink.isClosed()) {

			// Wait for a new PushMessage to be sent
			PushMessage msg = messageQueue.poll(WAIT_SECONDS, TimeUnit.SECONDS);

			if (msg == null) {

				LOGGER.trace("Sending keep alive to {}", userName);

				// Send a keep alive message to the client instead
				sink.send(createKeepAlive());
				continue;
			}

			LOGGER.trace("Sending message '{}' to {}", msg, userName);

			OutboundSseEvent event = createEvent(msg);
			sink.send(event)
					.exceptionally(this::logExceptionalResult);
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
	private <U> U logExceptionalResult(Throwable throwable) {

		LOGGER.atDebug()
				.setMessage("Sending to {} failed: {}")
				.addArgument(userName)
				.addArgument(throwable.getMessage())
				.log();
		return null;
	}

	@Context
	public void setSse(Sse sse) {
		this.sse = sse;
	}

	@Override
	public String forUserId() {
		return userId;
	}

	@Override
	public void sendMessage(PushMessage message) {
		messageQueue.add(message);
	}

	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
				.add("userId", userId)
				.add("userName", userName)
				.toString();
	}

}
