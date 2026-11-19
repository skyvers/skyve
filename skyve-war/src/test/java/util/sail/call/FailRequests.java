package util.sail.call;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.RequestInterception;
import util.sail.browser.RequestRule;
import util.sail.browser.RequestRule.ErrorReason;

/**
 * Fails the browser's requests to matching URLs with a network error before they are sent, as a dropped connection
 * would, until {@link ClearRequestRules}. Requests from pages and from service and shared workers are caught alike.
 * <ul>
 * <li>{@code url}: the URLs, as a glob over the whole URL: {@code *} matches any run of characters, {@code ?} any one,
 * 		and {@code \} escapes the next, such as {@code *}{@code /api/orders/*} (required)</li>
 * <li>{@code reason}: the error the page sees, such as {@code InternetDisconnected} (the default), {@code TimedOut} or
 * 		{@code ConnectionReset} (any {@link ErrorReason}, in any case, with or without underscores)</li>
 * <li>{@code method}: only this HTTP method, such as {@code POST}</li>
 * <li>{@code times}: only the next n matching requests; 0 (the default) for all of them</li>
 * </ul>
 * No variable. Needs {@code REQUEST_INTERCEPTION}.
 */
public final class FailRequests extends AbstractSailCall {
	/** Takes url, reason, method and times. */
	public FailRequests() {
		super("url", "reason", "method", "times");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		RequestRule rule = narrow(RequestRule.fail(parameters.required("url"),
													parameters.choice("reason", ErrorReason.class, ErrorReason.INTERNET_DISCONNECTED)),
									parameters);
		require(context, RequestInterception.class).addRule(rule);
		return null;
	}

	/**
	 * Narrows a rule by the {@code method} and {@code times} parameters that the request rule calls share.
	 *
	 * @param rule	the rule
	 * @param parameters	the call's parameters
	 * @return the rule for that method only, if one is given, and for that many requests
	 * @throws IllegalArgumentException	if {@code times} isn't a whole number of 0 or more
	 */
	static @Nonnull RequestRule narrow(@Nonnull RequestRule rule, @Nonnull SailCallParameters parameters) {
		RequestRule result = rule.times(parameters.integer("times", 0, 0));
		String method = parameters.optional("method");
		return (method == null) ? result : result.method(method.trim());
	}
}
