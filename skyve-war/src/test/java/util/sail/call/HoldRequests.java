package util.sail.call;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.RequestInterception;
import util.sail.browser.RequestRule;

/**
 * Holds the browser's requests to matching URLs before they reach the server, for a test of a slow or hung server,
 * until {@link ReleaseRequests} lets them go or fails them. Later matching requests are held too, until
 * {@link ClearRequestRules}. The call returns at once; follow it with {@link AwaitHeldRequests} to wait until the page
 * has sent one.
 * <ul>
 * <li>{@code url}: the URLs, as a glob over the whole URL, as for {@link FailRequests} (required)</li>
 * <li>{@code method}, {@code times}: as for {@link FailRequests}</li>
 * </ul>
 * No variable. Needs {@code REQUEST_INTERCEPTION}.
 */
public final class HoldRequests extends AbstractSailCall {
	/** Takes url, method and times. */
	public HoldRequests() {
		super("url", "method", "times");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		RequestRule rule = FailRequests.narrow(RequestRule.hold(parameters.required("url")), parameters);
		require(context, RequestInterception.class).addRule(rule);
		return null;
	}
}
