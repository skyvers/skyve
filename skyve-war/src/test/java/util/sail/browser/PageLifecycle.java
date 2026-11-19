package util.sail.browser;

import jakarta.annotation.Nonnull;

/**
 * Freezes and resumes tabs, as a browser does to a tab left in the background. A frozen page runs no script, hears no
 * message and answers nothing, but keeps its Web Locks; a driver command to it waits until it is resumed.
 * <p>
 * The capability {@link Capability#PAGE_LIFECYCLE}.
 */
public interface PageLifecycle {
	/**
	 * Freezes a tab. Freeze a tab that is not in front, and resume it before the test ends.
	 *
	 * @param windowHandle	the tab's WebDriver window handle
	 * @throws IllegalStateException	if the browser has no such tab
	 */
	void freeze(@Nonnull String windowHandle);

	/**
	 * Makes a frozen tab active again.
	 *
	 * @param windowHandle	the tab's WebDriver window handle
	 * @throws IllegalStateException	if the browser has no such tab
	 */
	void resume(@Nonnull String windowHandle);
}
