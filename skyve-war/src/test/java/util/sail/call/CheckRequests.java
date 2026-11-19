package util.sail.call;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.support.ui.FluentWait;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.LoggedRequest;
import util.sail.browser.RequestLog;
import util.sail.browser.SailBrowserControl;

/**
 * Checks the browser's request log (every request any page or worker of the leg's browser made since the browser's
 * control began watching, or the last {@code clear}): how many requests match, for a test of what the page did or
 * didn't send. The control begins watching the first time a step asks for it, so a check in the step where watching
 * began fails, as the log would hold nothing from the steps before: put a call before the steps to watch, such as
 * {@code RequestLog} with {@code clear=true}.
 * <p>
 * A request that a service worker answers for the page is logged as the page made it, marked as answered by the
 * worker, and, when the worker fetched it from the network, again as the worker made it. By default the page's marked
 * copy is left out, so such a request is counted once (and one the worker answered from its cache isn't counted). But
 * a request the worker forwards that fails in the network is counted twice: Chrome reports no answer for the page's
 * copy, so nothing marks it as the worker's (narrow {@code matching}, or count {@code failed} requests with that in
 * mind).
 * <ul>
 * <li>{@code matching}: a regular expression searched for in {@code <METHOD> <url>} (required), such as
 * 		{@code POST .*}{@code /media\?.*offset=0&}</li>
 * <li>{@code failed}: {@code true} to count only requests that failed in the network, {@code false} only those that
 * 		didn't; all of them by default</li>
 * <li>{@code exactly}, {@code atLeast}, {@code atMost}: the count wanted; at least 1 if none is given</li>
 * <li>{@code waitMillis}: how long to keep looking for the count wanted, for requests the page sends a little later;
 * 		0 (look once) by default</li>
 * <li>{@code clear}: {@code true} to forget the log afterwards, whether or not the check held</li>
 * <li>{@code ignoreServiceWorkerCopies}: {@code false} to count the page's copies of requests a service worker
 * 		answered as well; {@code true} by default</li>
 * </ul>
 * Its variable, if the step names one, is the number counted. On failure the message lists the matching requests and
 * the last few in the log. Needs {@code REQUEST_LOG}.
 */
public final class CheckRequests extends AbstractSailCall {
	/** How many of the last requests in the log a failure lists. */
	private static final int LAST = 8;
	private static final String FAILED = "failed";

	/** Takes matching, failed, exactly, atLeast, atMost, waitMillis, clear and ignoreServiceWorkerCopies. */
	public CheckRequests() {
		super("matching", FAILED, "exactly", "atLeast", "atMost", "waitMillis", "clear", "ignoreServiceWorkerCopies");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		Pattern pattern = pattern(parameters, "matching");
		Boolean failed = (parameters.optional(FAILED) == null) ? null : Boolean.valueOf(parameters.bool(FAILED, false));
		int exactly = parameters.integer("exactly", -1, 0);
		int atLeast = parameters.integer("atLeast", -1, 0);
		int atMost = parameters.integer("atMost", -1, 0);
		if ((exactly < 0) && (atLeast < 0) && (atMost < 0)) {
			atLeast = 1;
		}
		int waitMillis = parameters.integer("waitMillis", 0, 0);
		boolean clear = parameters.bool("clear", false);
		boolean ignoreCopies = parameters.bool("ignoreServiceWorkerCopies", true);
		Count wanted = new Count(exactly, atLeast, atMost);

		RequestLog log = require(context, RequestLog.class);
		AtomicReference<List<LoggedRequest>> all = new AtomicReference<>(log.requests());
		requireWatchingBefore(context, log.watchingSince(), "request log");
		List<LoggedRequest> counted = matching(all.get(), pattern, failed, ignoreCopies);
		if ((! wanted.holds(counted.size())) && (waitMillis > 0)) {
			// the event: the page sends the requests a little later. The log raises no event to wait on, so it is
			// checked every 100 ms, as Selenide.waitFor checks a page, until the count holds or the time is up
			try {
				new FluentWait<>(log).withTimeout(Duration.ofMillis(waitMillis))
										.pollingEvery(SailBrowserControl.POLLING)
										.until(l -> {
											List<LoggedRequest> now = l.requests();
											all.set(now);
											return Boolean.valueOf(wanted.holds(matching(now, pattern, failed, ignoreCopies).size()));
										});
			}
			catch (@SuppressWarnings("unused") TimeoutException e) {
				// the count still doesn't hold: reported below, with the log as it was last seen
			}
			counted = matching(all.get(), pattern, failed, ignoreCopies);
		}
		if (clear) {
			log.clearRequests();
		}
		int n = counted.size();
		if (! wanted.holds(n)) {
			List<LoggedRequest> last = all.get();
			throw new AssertionError(String.format("Expected requests matching /%s/ (%s), but %d matched: %s (the log holds %d requests; the last: %s)",
													pattern,
													wanted,
													Integer.valueOf(n),
													counted,
													Integer.valueOf(last.size()),
													last.subList(Math.max(0, last.size() - LAST), last.size())));
		}
		return String.valueOf(n);
	}

	/**
	 * Returns the requests in a log that match.
	 *
	 * <p>Complexity: O(r) in the number of requests, each a regular expression search.
	 *
	 * @param requests	the log
	 * @param pattern	searched for in {@code <METHOD> <url>}
	 * @param failed	true for failed requests only, false for those that didn't fail, null for both
	 * @param ignoreCopies	true to leave out the page's copies of requests a service worker answered
	 * @return the matching requests, in order
	 */
	private static @Nonnull List<LoggedRequest> matching(@Nonnull List<LoggedRequest> requests,
															@Nonnull Pattern pattern,
															@Nullable Boolean failed,
															boolean ignoreCopies) {
		List<LoggedRequest> result = new ArrayList<>();
		for (LoggedRequest request : requests) {
			if ((ignoreCopies && request.fromServiceWorker()) ||
					((failed != null) && ((request.failure() != null) != failed.booleanValue()))) {
				continue;
			}
			if (pattern.matcher(request.method() + ' ' + request.url()).find()) {
				result.add(request);
			}
		}
		return result;
	}

	/**
	 * Returns a parameter that is a regular expression.
	 *
	 * @param parameters	a call's parameters
	 * @param name	the parameter's name
	 * @return the expression, or null if the parameter is left out
	 * @throws IllegalArgumentException	if it isn't a regular expression, saying why
	 */
	static @Nullable Pattern optionalPattern(@Nonnull SailCallParameters parameters, @Nonnull String name) {
		String value = parameters.optional(name);
		return (value == null) ? null : compile(parameters, name, value);
	}

	/** Returns a required parameter that is a regular expression; see {@link #optionalPattern}. */
	private static @Nonnull Pattern pattern(@Nonnull SailCallParameters parameters, @Nonnull String name) {
		return compile(parameters, name, parameters.required(name));
	}

	/** Compiles a parameter's value as a regular expression, or says why it isn't one. */
	private static @Nonnull Pattern compile(@Nonnull SailCallParameters parameters, @Nonnull String name, @Nonnull String value) {
		try {
			return Pattern.compile(value);
		}
		catch (PatternSyntaxException e) {
			throw new IllegalArgumentException(String.format("%s's %s must be a regular expression, not %s: %s",
																parameters.call(), name, value, e.getDescription()), e);
		}
	}

	/**
	 * The count wanted: each bound is -1 when not given.
	 *
	 * @param exactly	the count exactly
	 * @param atLeast	the least
	 * @param atMost	the most
	 */
	private record Count(int exactly, int atLeast, int atMost) {
		boolean holds(int n) {
			return ((exactly < 0) || (n == exactly)) && ((atLeast < 0) || (n >= atLeast)) && ((atMost < 0) || (n <= atMost));
		}

		@Override
		public @Nonnull String toString() {
			List<String> bounds = new ArrayList<>(3);
			if (exactly >= 0) {
				bounds.add("exactly " + exactly);
			}
			if (atLeast >= 0) {
				bounds.add("at least " + atLeast);
			}
			if (atMost >= 0) {
				bounds.add("at most " + atMost);
			}
			return String.join(", ", bounds);
		}
	}
}
