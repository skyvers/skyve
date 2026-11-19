package util.sail.browser;

import java.util.Map;
import java.util.Set;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Says what {@link RequestInterception} does to the requests that match it: fail them, answer them, hold them, change
 * their headers, or lose their responses, at the request or the response stage, every time or a set number of times.
 * <p>
 * Make a rule with one of the static factories, then narrow it with {@link #method(String)}, {@link #atResponse()} and
 * {@link #times(int)}. Rules are immutable; each narrowing returns a new rule.
 *
 * @param urlPattern	the URLs it matches, as a glob over the whole URL: {@code *} matches any run of characters,
 * 						{@code ?} any one, and {@code \} escapes the next character
 * @param method	the HTTP method it matches (any case), or null for every method
 * @param stage	when the request is stopped to apply the rule
 * @param action	what is done to it
 * @param times	how many matching requests it applies to, or 0 for all of them
 */
public record RequestRule(@Nonnull String urlPattern, @Nullable String method, @Nonnull Stage stage, @Nonnull Action action, int times) {
	/** When a request is stopped: before it is sent, or once its response headers have arrived. */
	public enum Stage {
		/** Before the request is sent. */
		REQUEST,
		/** After the response headers arrive and before the body is read. */
		RESPONSE
	}

	/** The network errors a failed request can report to the page. */
	public enum ErrorReason {
		/** A generic failure. */
		FAILED,
		/** The request was aborted. */
		ABORTED,
		/** The request timed out. */
		TIMED_OUT,
		/** Access was denied. */
		ACCESS_DENIED,
		/** The connection was closed. */
		CONNECTION_CLOSED,
		/** The connection was reset. */
		CONNECTION_RESET,
		/** The connection was refused. */
		CONNECTION_REFUSED,
		/** The connection was aborted. */
		CONNECTION_ABORTED,
		/** The connection could not be made. */
		CONNECTION_FAILED,
		/** The host name could not be resolved. */
		NAME_NOT_RESOLVED,
		/** There is no network. */
		INTERNET_DISCONNECTED,
		/** The address can't be reached. */
		ADDRESS_UNREACHABLE,
		/** The browser blocked it. */
		BLOCKED_BY_CLIENT,
		/** The response blocked it. */
		BLOCKED_BY_RESPONSE
	}

	/** What is done to a matching request. */
	public sealed interface Action permits Fail, Fulfil, Hold, Headers {
		// a closed set of actions
	}

	/**
	 * Fails the request with a network error: before it is sent, as if the connection broke (at the request stage),
	 * or after the server has handled it, so the page never gets the answer (at the response stage: a lost response).
	 *
	 * @param reason	the error the page sees
	 */
	public record Fail(@Nonnull ErrorReason reason) implements Action {
	}

	/**
	 * Answers the request with this response instead of the server's (at the request stage), or replaces the server's
	 * response (at the response stage).
	 *
	 * @param status	the HTTP status
	 * @param headers	the response headers (the map is copied)
	 * @param body	the response body, sent as UTF-8
	 */
	public record Fulfil(int status, @Nonnull Map<String, String> headers, @Nonnull String body) implements Action {
		/**
		 * Copies the headers, so the rule can't change after it is added.
		 *
		 * @param status	the HTTP status
		 * @param headers	the response headers
		 * @param body	the response body
		 */
		public Fulfil {
			headers = Map.copyOf(headers);
		}
	}

	/**
	 * Holds the request before it is sent, until {@link RequestInterception#releaseHeld()} or
	 * {@link RequestInterception#failHeld(ErrorReason)}.
	 */
	public record Hold() implements Action {
	}

	/**
	 * Changes the headers: the request's (at the request stage) or the response's (at the response stage).
	 *
	 * @param set	headers to add or replace (the map is copied)
	 * @param remove	header names to remove, in any case (the set is copied)
	 */
	public record Headers(@Nonnull Map<String, String> set, @Nonnull Set<String> remove) implements Action {
		/**
		 * Copies the headers and names, so the rule can't change after it is added.
		 *
		 * @param set	headers to add or replace
		 * @param remove	header names to remove
		 */
		public Headers {
			set = Map.copyOf(set);
			remove = Set.copyOf(remove);
		}
	}

	/**
	 * Checks the rule.
	 *
	 * @param urlPattern	the URLs it matches
	 * @param method	the method it matches, or null for all
	 * @param stage	when the request is stopped
	 * @param action	what is done to it
	 * @param times	how many requests it applies to, or 0 for all
	 * @throws IllegalArgumentException	if the pattern is blank, {@code times} is negative, or the action can't be done
	 * 										at the stage (holding at the response stage)
	 */
	public RequestRule {
		if (urlPattern.isBlank()) {
			throw new IllegalArgumentException("A request rule needs a URL pattern");
		}
		if (times < 0) {
			throw new IllegalArgumentException("A request rule's times must be 0 (every time) or more, not " + times);
		}
		if ((stage == Stage.RESPONSE) && (action instanceof Hold)) {
			throw new IllegalArgumentException("A request rule can hold a request only at the request stage");
		}
	}

	/**
	 * Returns a rule that fails the matching requests with a network error, before they are sent.
	 *
	 * @param urlPattern	the URLs it matches
	 * @param reason	the error the page sees
	 * @return the rule
	 */
	public static @Nonnull RequestRule fail(@Nonnull String urlPattern, @Nonnull ErrorReason reason) {
		return new RequestRule(urlPattern, null, Stage.REQUEST, new Fail(reason), 0);
	}

	/**
	 * Returns a rule that lets the matching requests reach the server and be handled, then fails them in the browser
	 * with a network error instead of giving the page the answer: a connection that dropped after the server committed.
	 *
	 * @param urlPattern	the URLs it matches
	 * @param reason	the error the page sees
	 * @return the rule, at the response stage
	 */
	public static @Nonnull RequestRule loseResponse(@Nonnull String urlPattern, @Nonnull ErrorReason reason) {
		return new RequestRule(urlPattern, null, Stage.RESPONSE, new Fail(reason), 0);
	}

	/**
	 * Returns a rule that answers the matching requests itself, without the server.
	 *
	 * @param urlPattern	the URLs it matches
	 * @param status	the HTTP status
	 * @param headers	the response headers
	 * @param body	the response body
	 * @return the rule
	 */
	public static @Nonnull RequestRule fulfil(@Nonnull String urlPattern, int status, @Nonnull Map<String, String> headers, @Nonnull String body) {
		return new RequestRule(urlPattern, null, Stage.REQUEST, new Fulfil(status, headers, body), 0);
	}

	/**
	 * Returns a rule that holds the matching requests before they are sent, until they are released or failed.
	 *
	 * @param urlPattern	the URLs it matches
	 * @return the rule
	 */
	public static @Nonnull RequestRule hold(@Nonnull String urlPattern) {
		return new RequestRule(urlPattern, null, Stage.REQUEST, new Hold(), 0);
	}

	/**
	 * Returns a rule that changes the matching requests' headers before they are sent.
	 *
	 * @param urlPattern	the URLs it matches
	 * @param set	headers to add or replace
	 * @param remove	header names to remove
	 * @return the rule
	 */
	public static @Nonnull RequestRule headers(@Nonnull String urlPattern, @Nonnull Map<String, String> set, @Nonnull Set<String> remove) {
		return new RequestRule(urlPattern, null, Stage.REQUEST, new Headers(set, remove), 0);
	}

	/**
	 * Returns this rule for one HTTP method only.
	 *
	 * @param name	the method, such as {@code POST}
	 * @return the narrowed rule
	 */
	public @Nonnull RequestRule method(@Nonnull String name) {
		return new RequestRule(urlPattern, name, stage, action, times);
	}

	/**
	 * Returns this rule applied once the response headers have arrived: a fulfil replaces the server's response, a
	 * headers rule changes the response's headers, and a fail loses the response.
	 *
	 * @return the rule at the response stage
	 * @throws IllegalArgumentException	if this rule holds requests
	 */
	public @Nonnull RequestRule atResponse() {
		return new RequestRule(urlPattern, method, Stage.RESPONSE, action, times);
	}

	/**
	 * Returns this rule applied to the next {@code count} matching requests only.
	 *
	 * @param count	how many, or 0 for all of them
	 * @return the limited rule
	 * @throws IllegalArgumentException	if the count is negative
	 */
	public @Nonnull RequestRule times(int count) {
		return new RequestRule(urlPattern, method, stage, action, count);
	}

	/**
	 * Indicates whether a stopped request matches this rule, leaving aside how many times it has applied.
	 *
	 * <p>Complexity: O(p &times; u) in the worst case, in the lengths of the pattern and the URL.
	 *
	 * @param url	the request's URL
	 * @param requestMethod	the request's method
	 * @param stoppedAt	the stage it is stopped at
	 * @return true if the stage, the method and the URL pattern all match
	 */
	public boolean matches(@Nonnull String url, @Nonnull String requestMethod, @Nonnull Stage stoppedAt) {
		return (stage == stoppedAt) &&
				((method == null) || method.equalsIgnoreCase(requestMethod)) &&
				globMatches(urlPattern, url);
	}

	/**
	 * Matches a URL glob against the whole of a text: {@code *} is any run of characters, {@code ?} is any one, and
	 * {@code \} escapes the next character.
	 *
	 * @param pattern	the glob
	 * @param text	the text, such as a URL
	 * @return true if the glob matches all of the text
	 */
	static boolean globMatches(@Nonnull String pattern, @Nonnull String text) {
		int p = 0;
		int t = 0;
		int star = -1; // the pattern position after the last *
		int starText = -1; // where in the text that * started matching
		while (t < text.length()) {
			if ((p < pattern.length()) && (pattern.charAt(p) == '*')) {
				star = ++p;
				starText = t;
			}
			else {
				int width = (p < pattern.length()) ? matchedWidth(pattern, p, text.charAt(t)) : 0;
				if (width > 0) {
					p += width;
					t++;
				}
				else {
					if (star < 0) {
						return false;
					}
					p = star; // let the last * take one more character
					t = ++starText;
				}
			}
		}
		while ((p < pattern.length()) && (pattern.charAt(p) == '*')) {
			p++;
		}
		return p == pattern.length();
	}

	/**
	 * Returns how much of a glob, at a position that isn't {@code *}, one character of the text matches: 1 for a
	 * literal or {@code ?}, 2 for a literal escaped with {@code \}, or 0 if it doesn't match.
	 */
	private static int matchedWidth(@Nonnull String pattern, int p, char c) {
		char g = pattern.charAt(p);
		if ((g == '\\') && (p + 1 < pattern.length())) {
			return (pattern.charAt(p + 1) == c) ? 2 : 0;
		}
		return ((g == '?') || (g == c)) ? 1 : 0;
	}
}
