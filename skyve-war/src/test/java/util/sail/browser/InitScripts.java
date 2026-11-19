package util.sail.browser;

import jakarta.annotation.Nonnull;

/**
 * Scripts that run in every document the browser's pages load, before the page's own scripts.
 * <p>
 * The capability {@link Capability#INIT_SCRIPTS}.
 */
public interface InitScripts {
	/**
	 * Runs a script in every document the browser's pages load from now on, before the page's own scripts, including
	 * after a reload and in pages a service worker serves. It doesn't run in the documents open now. A script already
	 * added is not added again. Scripts last until the browser stops.
	 *
	 * @param source	the script
	 */
	void addInitScript(@Nonnull String source);
}
