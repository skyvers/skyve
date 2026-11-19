package util.sail.browser;

import jakarta.annotation.Nonnull;

/**
 * Key presses on whatever has the focus, as a keyboard makes them: trusted key events, which a page script can't make.
 * <p>
 * The capability {@link Capability#KEYS}.
 */
public interface Keys {
	/**
	 * Presses a key, or a chord: every key but the last is held down while the last is pressed, then they are released
	 * in the opposite order. For example {@code press(org.openqa.selenium.Keys.ESCAPE)}, or
	 * {@code press(org.openqa.selenium.Keys.CONTROL, "a")}.
	 *
	 * @param keys	the keys: {@link org.openqa.selenium.Keys} values, or characters
	 * @throws IllegalArgumentException	if no key is given
	 */
	void press(@Nonnull CharSequence... keys);
}
