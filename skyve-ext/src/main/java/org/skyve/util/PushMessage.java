package org.skyve.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.skyve.CORE;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.user.User;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.MoreObjects;

import jakarta.annotation.Nonnull;

/**
 * Use this class to specify a push message to send to connected client user interfaces.
 * By default the message will be broadcast to all currently connected users of the system.
 * Use the {@link #user()} and overloaded variants to constrain the message to a subset of connected users.
 * If a user is specified that is not currently using the system, the push message is not sent.
 */
public class PushMessage {
	private static final Logger LOGGER = LoggerFactory.getLogger(PushMessage.class);

	private static final String ITEM_TYPE = "type";
	private static final String ITEM_SEVERITY = "severity";
	private static final String ITEM_MESSAGE = "message";
	private static final String ITEM_METHOD = "method";
	private static final String ITEM_ARGUMENT = "argument";
	
	/**
	 * Holds the connected clients.
	 */
	public static final ConcurrentLinkedQueue<PushMessageReceiver> RECEIVERS = new ConcurrentLinkedQueue<>();

	private static ScheduledExecutorService reaperExecutor;
	private static ScheduledFuture<?> reaperFuture;

	/**
	 * Start a periodic reaper that removes stale receivers from {@link #RECEIVERS}.
	 * Stale receivers are those whose {@link PushMessageReceiver#isStale()} method returns {@code true}.
	 * The reaper also closes stale receivers via {@link PushMessageReceiver#close()}.
	 * 
	 * @param intervalInSeconds How often (in seconds) to scan for stale receivers.
	 */
	public static synchronized void startReaper(int intervalInSeconds) {
		if (reaperFuture != null) {
			LOGGER.warn("Push message reaper already running – ignoring duplicate start request");
			return;
		}

		reaperExecutor = Executors.newSingleThreadScheduledExecutor(r -> {
			Thread t = new Thread(r, "push-receiver-reaper");
			t.setDaemon(true);
			return t;
		});

		reaperFuture = reaperExecutor.scheduleAtFixedRate(() -> {
			int removed = 0;
			for (PushMessageReceiver receiver : RECEIVERS) {
				boolean stale;
				try {
					stale = receiver.isStale();
				} catch (RuntimeException e) {
					// Defensive: avoid killing future reaper runs on a broken receiver implementation.
					LOGGER.warn("Receiver {} threw during isStale(); removing defensively", receiver, e);
					stale = true;
				}

				if (!stale) {
					continue;
				}

				if (RECEIVERS.remove(receiver)) {
					removed++;
				}
				try {
					receiver.close();
				} catch (@SuppressWarnings("unused") Exception e) {
					// Best effort – the receiver may already be closed
				}
			}
			if (removed > 0) {
				LOGGER.info("Push message reaper removed {} stale receiver(s), {} remaining",
						Integer.valueOf(removed), Integer.valueOf(RECEIVERS.size()));
			}
		}, intervalInSeconds, intervalInSeconds, TimeUnit.SECONDS);

		LOGGER.info("Push message reaper started with interval {}s", Integer.valueOf(intervalInSeconds));
	}

	/**
	 * Stop the stale-receiver reaper. Safe to call even if not running.
	 * <p>
	 * Uses {@code shutdownNow()} followed by {@code awaitTermination()} rather than
	 * a plain {@code shutdown()} to ensure the reaper thread has fully terminated
	 * before the servlet context finishes destroying. Without this, a reaper thread
	 * that is mid-execution during WAR undeploy/redeploy can hold a reference to
	 * the old WAR's classloader, preventing it (and all classes loaded through it)
	 * from being garbage collected — a classloader leak.
	 */
	public static synchronized void stopReaper() {
		if (reaperFuture != null) {
			reaperFuture.cancel(false);
			reaperFuture = null;
		}
		if (reaperExecutor != null) {
			reaperExecutor.shutdownNow();
			try {
				if (!reaperExecutor.awaitTermination(5, TimeUnit.SECONDS)) {
					LOGGER.warn("Push message reaper thread did not terminate within 5 seconds");
				}
			} catch (@SuppressWarnings("unused") InterruptedException e) {
				Thread.currentThread().interrupt();
			}
			reaperExecutor = null;
		}
		LOGGER.info("Push message reaper stopped");
	}

	private Set<String> userIds = new TreeSet<>();
	private List<Map<String, Object>> items = new ArrayList<>();

	/** 
	 * For the current user
	 */
	public @Nonnull PushMessage user() {
		userIds.add(CORE.getUser().getId());
		return this;
	}
	
	/**
	 * For another user
	 */
	public @Nonnull PushMessage user(@Nonnull String userId) {
		userIds.add(userId);
		return this;
	}
	
	/**
	 * For another user
	 */
	public @Nonnull PushMessage user(@Nonnull User user) {
		userIds.add(user.getId());
		return this;
	}
	
	/**
	 * Get the user IDs this message will go to.
	 */
	public @Nonnull Set<String> getUserIds() {
		return userIds;
	}
	
	/**
	 * Get the items in this push message.
	 * @return A set of JSON command objects such as growl, message, rerender, execute destined for a connected client.
	 */
	public @Nonnull List<Map<String, Object>> getItems() {
		return items;
	}

	/**
	 *  Put up a growl
	 */
	public @Nonnull PushMessage growl(@Nonnull MessageSeverity severity, @Nonnull String message) {
		Map<String, Object> item = new TreeMap<>();
		item.put(ITEM_TYPE, "g");
		item.put(ITEM_SEVERITY, severity.toString());
		item.put(ITEM_MESSAGE, Util.nullSafeI18n(message));
		items.add(item);
		return this;
	}
	
	/**
	 *  Put up a message
	 */
	public @Nonnull PushMessage message(@Nonnull MessageSeverity severity, @Nonnull String message) {
		Map<String, Object> item = new TreeMap<>();
		item.put(ITEM_TYPE, "m");
		item.put(ITEM_SEVERITY, severity.toString());
		item.put(ITEM_MESSAGE, Util.nullSafeI18n(message));
		items.add(item);
		return this;
	}

	/**
	 *  Rerender the current view with no client validation
	 */
	public @Nonnull PushMessage rerender() {
		Map<String, Object> item = new TreeMap<>();
		item.put(ITEM_TYPE, "r");
		items.add(item);
		return this;
	}

	/**
	 * Execute some javascript function
	 */
	public @Nonnull PushMessage execute(@Nonnull String javascriptWindowFunctionName,
											@Nonnull Map<String, Object> argumentJSON) {
		Map<String, Object> item = new TreeMap<>();
		item.put(ITEM_TYPE, "j");
		item.put(ITEM_METHOD, javascriptWindowFunctionName);
		item.put(ITEM_ARGUMENT, argumentJSON);
		items.add(item);
		return this;
	}

	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
				.add("userIds", userIds)
				.add("items", items)
				.toString();
	}

	/**
	 * This interface is to be implemented by receivers of PushMessages. Those
	 * receivers need to register themselves by being added to
	 * <code>org.skyve.util.PushMessage.RECEIVERS</code>, and removing themselves
	 * once no longer needed.
	 */
	public static interface PushMessageReceiver {

		/**
		 * Which user is this receiver for? Broadcast messages
		 * will be sent to this receiver regardless of the 
		 * return value here.
		 * 
		 * @return The user's UUID
		 */
		public String forUserId();

		/**
		 * Send the given PushMessage to the receiver's client.
		 * 
		 * @param message The message to send.
		 */
		public void sendMessage(PushMessage message);

		/**
		 * Whether this receiver is stale and should be removed by the reaper.
		 * Implementations should return {@code true} when the receiver's underlying
		 * connection is no longer viable (e.g. last successful send was too long ago).
		 * 
		 * @return {@code true} if this receiver should be reaped.
		 */
		default boolean isStale() {
			return false;
		}

		/**
		 * Close the receiver's underlying resources. Called by the reaper
		 * when a stale receiver is removed from {@link PushMessage#RECEIVERS}.
		 */
		default void close() {
			// no-op by default
		}
	}
}
