package util.sail.call;

import org.openqa.selenium.Dimension;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.Window;

/**
 * Sets the size of the leg's browser window, as a user dragging its edge would; a page script can't resize a window it
 * didn't open. For a page that chooses its layout from the window's width.
 * <ul>
 * <li>{@code width}: the window's width in pixels, 200 or more (required)</li>
 * <li>{@code height}: its height in pixels, 200 or more (required)</li>
 * </ul>
 * Its variable, if the step names one, is the window's size afterwards as {@code <width>x<height>}, which the platform
 * may have limited. Needs {@code WINDOW}, which every browser has.
 */
public final class ResizeWindow extends AbstractSailCall {
	/** Takes width and height. */
	public ResizeWindow() {
		super("width", "height");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		int width = parameters.requiredInteger("width", 200);
		int height = parameters.requiredInteger("height", 200);
		Dimension size = require(context, Window.class).resizeWindow(width, height);
		return size.getWidth() + "x" + size.getHeight();
	}
}
