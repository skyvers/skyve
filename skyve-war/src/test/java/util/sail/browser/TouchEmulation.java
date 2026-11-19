package util.sail.browser;

/**
 * A touch screen on every page, so {@code (pointer: coarse)} matches and {@code navigator.maxTouchPoints} is set.
 * <p>
 * The capability {@link Capability#TOUCH}.
 */
public interface TouchEmulation {
	/**
	 * Emulates a touch screen on every page, now and as they open, or turns it off.
	 *
	 * @param maxTouchPoints	the touch points, or 0 for none (a mouse)
	 */
	void emulateTouch(int maxTouchPoints);
}
