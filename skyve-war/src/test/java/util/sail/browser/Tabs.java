package util.sail.browser;

import java.time.Duration;
import java.util.function.Supplier;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * The browser's tabs, which share its profile (cookies, storage and service workers), for tests of several tabs. The
 * SAIL contexts stay with the tab the automation started in, so work in another tab with script steps and calls only,
 * and come back before the next gesture.
 * <p>
 * The capability {@link Capability#TABS}.
 */
public interface Tabs {
	/**
	 * Opens a new tab in front and loads a URL in it.
	 *
	 * <p>Side effects: the driver drives the new tab from now on.
	 *
	 * @param url	the absolute URL
	 */
	void openTab(@Nonnull String url);

	/**
	 * Brings a tab to the front, waiting for it to appear if it hasn't yet, as one a link with {@code target="_blank"}
	 * opens.
	 *
	 * <p>Side effects: the driver drives that tab from now on.
	 *
	 * @param index	the tab, 0 being the first (the one opened first)
	 * @param timeout	how long to wait at most for the tab to appear; the wait ends as soon as it does
	 * @throws AssertionError	if the browser doesn't have that many tabs when the time is up
	 */
	void switchToTab(int index, @Nonnull Duration timeout);

	/**
	 * Closes the tab in front and brings the first tab to the front.
	 *
	 * <p>Side effects: the driver drives the first tab from now on.
	 */
	void closeTab();

	/**
	 * Returns how many tabs are open.
	 *
	 * @return the count
	 */
	int tabCount();

	/**
	 * Visits a tab the page opened (a link with {@code target="_blank"}) and comes back, within one step: waits for a tab
	 * other than the one in front to appear, brings it to the front, does the visit, closes the tab if asked, and brings
	 * back the tab the visit started from, even when the visit throws. Use it when the next step must find the starting
	 * tab in front, as a SmartClient leg, which waits for SmartClient in the tab in front after every step, does.
	 *
	 * @param <T>	the visit's result type
	 * @param timeout	how long to wait at most for the tab to appear; the wait ends as soon as it does
	 * @param close	true to close the visited tab afterwards, false to leave it open
	 * @param visit	what to do in the new tab, with the driver driving it
	 * @return what the visit returned
	 * @throws AssertionError	if no other tab is open when the time is up; or what the visit threw
	 */
	@Nullable <T> T visitNewTab(@Nonnull Duration timeout, boolean close, @Nonnull Supplier<T> visit);
}
