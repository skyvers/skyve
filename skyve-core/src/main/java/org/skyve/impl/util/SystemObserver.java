package org.skyve.impl.util;

/**
 * Enables observing skyve application events - currently startup and shutdown.
 * @author mike
 */
public interface SystemObserver {
	/**
	 * Called when starting up a skyve application.
	 */
	void startup();

	/**
	 * Called when shutting down a skyve application.
	 */
	void shutdown();
}
