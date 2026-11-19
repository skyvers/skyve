package util.sail.browser;

import java.util.List;

import jakarta.annotation.Nonnull;

/**
 * The requests that the browser's pages and workers made, since the browser started or the log was last cleared.
 * <p>
 * The capability {@link Capability#REQUEST_LOG}.
 */
public interface RequestLog {
	/**
	 * Returns the requests logged so far. A request that a service worker answers from the network is logged twice:
	 * as the page made it ({@link LoggedRequest#fromServiceWorker()}) and as the worker did.
	 *
	 * @return the requests, in the order they started; a copy the caller owns
	 */
	@Nonnull List<LoggedRequest> requests();

	/**
	 * Forgets the requests logged so far.
	 */
	void clearRequests();
}
