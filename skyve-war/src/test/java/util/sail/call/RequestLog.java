package util.sail.call;

import java.util.List;
import java.util.regex.Pattern;

import org.skyve.util.OWASP;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.LoggedRequest;

/**
 * Reads the browser's request log: the requests the leg's pages and workers made since the browser's control began
 * watching, or the log was last cleared, for a script step to check what the page sent and in what order. A request
 * that a service worker answers for the page is logged as the page made it ({@code fromServiceWorker} true) and, when
 * the worker fetched it from the network, again as the worker made it (see {@link CheckRequests}).
 * <ul>
 * <li>{@code matching}: a regular expression searched for in {@code <METHOD> <url>}; every request by default</li>
 * <li>{@code clear}: {@code true} to forget the log after reading it</li>
 * </ul>
 * Its variable is the matching requests as a JSON array, in the order they started, of objects with {@code method},
 * {@code url}, {@code status} (null while a request has no answer, or when it failed), {@code failure} (the network
 * error's text, or null) and {@code fromServiceWorker}; a script reads it with {@code JSON.parse(sail.vars.name)}.
 * Needs {@code REQUEST_LOG}.
 */
public final class RequestLog extends AbstractSailCall {
	/** Takes matching and clear. */
	public RequestLog() {
		super("matching", "clear");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		Pattern pattern = CheckRequests.optionalPattern(parameters, "matching");
		boolean clear = parameters.bool("clear", false);
		// this class's name is the interface's too
		util.sail.browser.RequestLog log = require(context, util.sail.browser.RequestLog.class);
		List<LoggedRequest> requests = log.requests();
		StringBuilder json = new StringBuilder(2 + requests.size() * 128).append('[');
		for (LoggedRequest request : requests) {
			if ((pattern == null) || pattern.matcher(request.method() + ' ' + request.url()).find()) {
				if (json.length() > 1) {
					json.append(',');
				}
				Integer status = request.status();
				json.append("{\"method\":").append(quote(request.method()))
					.append(",\"url\":").append(quote(request.url()))
					.append(",\"status\":").append((status == null) ? "null" : status.toString())
					.append(",\"failure\":").append(quote(request.failure()))
					.append(",\"fromServiceWorker\":").append(request.fromServiceWorker())
					.append('}');
			}
		}
		if (clear) {
			log.clearRequests();
		}
		json.append(']');
		return json.toString();
	}

	/** Returns text as a JSON string, or {@code null}. */
	private static @Nonnull String quote(@Nullable String text) {
		return (text == null) ? "null" : ('"' + OWASP.escapeJsonString(text) + '"');
	}
}
