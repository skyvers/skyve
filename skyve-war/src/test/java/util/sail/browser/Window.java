package util.sail.browser;

import org.openqa.selenium.Dimension;

import jakarta.annotation.Nonnull;

/**
 * The browser window, as a user changes it, which a page script can't do to a window it didn't open.
 * <p>
 * The capability {@link Capability#WINDOW}.
 */
public interface Window {
	/**
	 * Sets the size of the browser window, as a user dragging its edge would.
	 *
	 * @param width	the window's width in pixels
	 * @param height	its height in pixels
	 * @return the window's size afterwards, which the platform may have limited
	 */
	@Nonnull Dimension resizeWindow(int width, int height);
}
