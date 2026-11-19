package util.sail.browser;

import java.time.Duration;

import jakarta.annotation.Nonnull;

/**
 * The network as the whole browser sees it: its pages and every service, shared and dedicated worker, now and as
 * they start. Setting a page alone offline isn't enough, as a page's service worker still reaches the network through
 * its own {@code fetch}. The conditions last until changed or until the browser stops.
 * <p>
 * The capability {@link Capability#NETWORK_CONDITIONS}.
 */
public interface NetworkConditions {
	/**
	 * Takes the whole browser offline: every request fails as if there were no network.
	 */
	void offline();

	/**
	 * Brings the whole browser back online, at full speed.
	 */
	void online();

	/**
	 * Slows the whole browser to a weak signal.
	 *
	 * @param latency	the extra round trip time
	 * @param downloadBytesPerSecond	the download limit, or -1 for none
	 * @param uploadBytesPerSecond	the upload limit, or -1 for none
	 */
	void throttle(@Nonnull Duration latency, long downloadBytesPerSecond, long uploadBytesPerSecond);
}
