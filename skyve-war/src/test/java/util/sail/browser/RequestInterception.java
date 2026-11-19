package util.sail.browser;

import java.time.Duration;

import jakarta.annotation.Nonnull;

/**
 * Rules for the requests of every page and worker of the browser (service and shared workers included, so a request
 * a worker forwards is caught too): fail them, hold them, answer them, change their headers, or lose their responses.
 * Rules last until {@link #clearRules()} or until the browser stops.
 * <p>
 * The capability {@link Capability#REQUEST_INTERCEPTION}.
 */
public interface RequestInterception {
	/**
	 * Adds a rule. Rules are tried in the order they were added; the first that matches a request, and hasn't used up
	 * its {@link RequestRule#times()}, applies to it. Requests that no rule takes carry on unchanged.
	 *
	 * <p>Side effects: from now on the browser stops matching requests (in every page and worker, now and as they start)
	 * to apply the rule.
	 *
	 * @param rule	the rule
	 */
	void addRule(@Nonnull RequestRule rule);

	/**
	 * Removes every rule and lets held requests carry on first.
	 *
	 * <p>Side effects: the browser stops stopping requests.
	 */
	void clearRules();

	/**
	 * Returns how many requests are being held by a {@link RequestRule.Hold} rule. A request that a page sent is no
	 * longer held once the page has gone to another document (it navigated or reloaded) or closed, as far as the browser
	 * says so. One that an iframe in the page sent stays counted when the page goes, until the held requests are let go.
	 *
	 * @return the count
	 */
	int heldCount();

	/**
	 * Waits until at least a number of requests are being held: a condition wait on the page sending them.
	 *
	 * @param count	the number, at least 1
	 * @param timeout	how long to wait at most; the wait ends as soon as enough are held
	 * @throws AssertionError	if fewer are held when the time is up
	 */
	void awaitHeld(int count, @Nonnull Duration timeout);

	/**
	 * Lets every held request carry on to the server. A request the page has given up on in the meantime is skipped.
	 * Rules stay in place: later matching requests are held too.
	 */
	void releaseHeld();

	/**
	 * Fails every held request with a network error. Rules stay in place: later matching requests are held too.
	 *
	 * @param reason	the error the page sees
	 */
	void failHeld(@Nonnull RequestRule.ErrorReason reason);
}
