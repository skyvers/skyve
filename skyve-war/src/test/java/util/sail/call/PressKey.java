package util.sail.call;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.Keys;

/**
 * Presses a key on whatever has the focus, as a keyboard does: a trusted key event, which a page script can't make, and
 * which counts as the user using the page.
 * <ul>
 * <li>{@code key}: a Selenium {@code Keys} name, such as {@code ESCAPE}, {@code ENTER}, {@code TAB} or
 * 		{@code ARROW_DOWN}, in any case (required)</li>
 * <li>{@code count}: how many times; 1 by default</li>
 * </ul>
 * No variable. Needs {@code KEYS}, which every browser has.
 */
public final class PressKey extends AbstractSailCall {
	/** Takes key and count. */
	public PressKey() {
		super("key", "count");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		parameters.required("key");
		org.openqa.selenium.Keys key = parameters.choice("key", org.openqa.selenium.Keys.class, org.openqa.selenium.Keys.ESCAPE);
		int count = parameters.integer("count", 1, 1);
		Keys keys = require(context, Keys.class);
		for (int i = 0; i < count; i++) {
			keys.press(key);
		}
		return null;
	}
}
