package util.deployed.transport;

import java.time.Instant;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Records bounded, correlation-scoped events for deployed integration tests.
 *
 * <p>This class exists only in the classified deployed-test overlay. Feature suites register
 * their own event vocabulary and call {@link #recordEvent(String, String)} from overlay hooks.
 */
public final class DeployedItEventRecorder {
	private static final int MAX_CORRELATIONS = 64;
	private static final int MAX_EVENTS_PER_CORRELATION = 256;
	private static final Map<String, Deque<Event>> EVENTS = new LinkedHashMap<>();

	private DeployedItEventRecorder() {
		// Utility class.
	}

	/** Starts or clears one server-authorised correlation. */
	public static synchronized void start(String correlationId) {
		if ((! EVENTS.containsKey(correlationId)) && (EVENTS.size() >= MAX_CORRELATIONS)) {
			String eldest = EVENTS.keySet().iterator().next();
			EVENTS.remove(eldest);
		}
		EVENTS.put(correlationId, new ArrayDeque<>(MAX_EVENTS_PER_CORRELATION));
	}

	/** Records an event only when its correlation was started by the authenticated probe. */
	public static synchronized void recordEvent(String correlationId, String eventName) {
		Deque<Event> events = EVENTS.get(correlationId);
		if (events == null) {
			return;
		}
		if (events.size() == MAX_EVENTS_PER_CORRELATION) {
			events.removeFirst();
		}
		events.addLast(new Event(Instant.now().toString(), eventName));
	}

	/** Returns a stable snapshot in recording order. */
	public static synchronized List<Event> snapshot(String correlationId) {
		Deque<Event> events = EVENTS.get(correlationId);
		return (events == null) ? List.of() : new ArrayList<>(events);
	}

	/** Removes one completed correlation. */
	public static synchronized void remove(String correlationId) {
		EVENTS.remove(correlationId);
	}

	/** Represents one generic event without imposing feature vocabulary. */
	public record Event(String timestamp, String name) {
		// Immutable event value.
	}
}
