package util.sail.call;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.RequestInterception;
import util.sail.browser.RequestRule;
import util.sail.browser.RequestRule.ErrorReason;

/**
 * Lets the matching requests reach the server and be handled, then fails them in the browser with a network error
 * instead of giving the page the answer: a connection that dropped after the server committed. Until
 * {@link ClearRequestRules}.
 * <ul>
 * <li>{@code url}: the URLs, as a glob over the whole URL, as for {@link FailRequests} (required)</li>
 * <li>{@code reason}: the error the page sees, such as {@code ConnectionReset} (the default) or {@code TimedOut} (any
 * 		{@link ErrorReason})</li>
 * <li>{@code method}, {@code times}: as for {@link FailRequests}</li>
 * </ul>
 * No variable. Needs {@code REQUEST_INTERCEPTION}.
 */
public final class LoseResponses extends AbstractSailCall {
	/** Takes url, reason, method and times. */
	public LoseResponses() {
		super("url", "reason", "method", "times");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		RequestRule rule = FailRequests.narrow(RequestRule.loseResponse(parameters.required("url"),
																		parameters.choice("reason", ErrorReason.class, ErrorReason.CONNECTION_RESET)),
												parameters);
		require(context, RequestInterception.class).addRule(rule);
		return null;
	}
}
