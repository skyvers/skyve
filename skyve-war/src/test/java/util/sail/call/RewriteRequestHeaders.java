package util.sail.call;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.RequestInterception;
import util.sail.browser.RequestRule;

/**
 * Changes the headers of the browser's requests to matching URLs before they reach the server, until
 * {@link ClearRequestRules}: for a page that sends an old client version, or no CSRF token.
 * <ul>
 * <li>{@code url}: the URLs, as a glob over the whole URL, as for {@link FailRequests} (required)</li>
 * <li>{@code set}: headers to add or replace, as {@code Name=value} pairs separated by {@code ;}</li>
 * <li>{@code remove}: header names to remove, in any case, comma separated</li>
 * <li>{@code method}, {@code times}: as for {@link FailRequests}</li>
 * </ul>
 * It needs {@code set} or {@code remove}, or both. No variable. Needs {@code REQUEST_INTERCEPTION}.
 */
public final class RewriteRequestHeaders extends AbstractSailCall {
	/** Takes url, set, remove, method and times. */
	public RewriteRequestHeaders() {
		super("url", "set", "remove", "method", "times");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		String url = parameters.required("url");
		Map<String, String> set = new LinkedHashMap<>();
		String pairs = parameters.optional("set");
		if (pairs != null) {
			for (String pair : pairs.split(";")) {
				if (pair.isBlank()) {
					continue;
				}
				int equals = pair.indexOf('=');
				if ((equals < 0) || pair.substring(0, equals).isBlank()) {
					throw new IllegalArgumentException(name() + "'s set must be Name=value pairs separated by ;, not " + pairs);
				}
				set.put(pair.substring(0, equals).trim(), pair.substring(equals + 1).trim());
			}
		}
		List<String> names = parameters.list("remove");
		if (set.isEmpty() && names.isEmpty()) {
			throw new IllegalArgumentException(name() + " needs headers to set or remove");
		}
		Set<String> remove = new HashSet<>(names);
		RequestRule rule = FailRequests.narrow(RequestRule.headers(url, set, remove), parameters);
		require(context, RequestInterception.class).addRule(rule);
		return null;
	}
}
