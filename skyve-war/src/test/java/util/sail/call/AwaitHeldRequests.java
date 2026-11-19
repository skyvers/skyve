package util.sail.call;

import java.time.Duration;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.RequestInterception;

/**
 * Waits until the browser holds some requests (see {@link HoldRequests}), and fails if it doesn't in time. The wait
 * ends as soon as enough are held.
 * <ul>
 * <li>{@code count}: how many, at least; 1 by default</li>
 * <li>{@code timeoutMillis}: how long to wait at most; the leg's timeout ({@code sail.timeoutSeconds}) by default</li>
 * </ul>
 * Its variable, if the step names one, is the number held. Needs {@code REQUEST_INTERCEPTION}.
 */
public final class AwaitHeldRequests extends AbstractSailCall {
	private static final String TIMEOUT_MILLIS = "timeoutMillis";

	/** Takes count and timeoutMillis. */
	public AwaitHeldRequests() {
		super("count", TIMEOUT_MILLIS);
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		int count = parameters.integer("count", 1, 1);
		String timeoutMillis = parameters.optional(TIMEOUT_MILLIS);
		Duration timeout = (timeoutMillis == null) ? context.timeout() : Duration.ofMillis(parameters.integer(TIMEOUT_MILLIS, 1, 1));
		RequestInterception interception = require(context, RequestInterception.class);
		interception.awaitHeld(count, timeout);
		return String.valueOf(interception.heldCount());
	}
}
