package util.sail.call;

import java.util.Map;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.RequestInterception;
import util.sail.browser.RequestRule;

/**
 * Answers the browser's requests to matching URLs itself, without the server, or replaces the server's answer, until
 * {@link ClearRequestRules}: for a server that is down (503), refuses (409), or says something odd.
 * <ul>
 * <li>{@code url}: the URLs, as a glob over the whole URL, as for {@link FailRequests} (required)</li>
 * <li>{@code status}: the HTTP status, 100 to 599 (required)</li>
 * <li>{@code body}: the response body, sent as UTF-8; empty by default</li>
 * <li>{@code contentType}: its type; {@code application/json} by default</li>
 * <li>{@code atResponse}: {@code true} to let the request reach the server and replace its answer; {@code false} (the
 * 		default) to answer before it is sent</li>
 * <li>{@code method}, {@code times}: as for {@link FailRequests}</li>
 * </ul>
 * No variable. Needs {@code REQUEST_INTERCEPTION}.
 */
public final class RespondToRequests extends AbstractSailCall {
	/** The largest HTTP status Chrome answers a request with. */
	private static final int MAXIMUM_STATUS = 599;

	/** Takes url, status, body, contentType, atResponse, method and times. */
	public RespondToRequests() {
		super("url", "status", "body", "contentType", "atResponse", "method", "times");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		String body = parameters.optional("body");
		String contentType = parameters.optional("contentType");
		int status = parameters.requiredInteger("status", 100);
		if (status > MAXIMUM_STATUS) {
			// Chrome refuses it, and the page's request would wait on
			throw new IllegalArgumentException(String.format("%s's status must be %d or less, not %d",
																name(), Integer.valueOf(MAXIMUM_STATUS), Integer.valueOf(status)));
		}
		RequestRule rule = RequestRule.fulfil(parameters.required("url"),
												status,
												Map.of("Content-Type", (contentType == null) ? "application/json" : contentType.trim()),
												(body == null) ? "" : body);
		if (parameters.bool("atResponse", false)) {
			rule = rule.atResponse();
		}
		rule = FailRequests.narrow(rule, parameters);
		require(context, RequestInterception.class).addRule(rule);
		return null;
	}
}
