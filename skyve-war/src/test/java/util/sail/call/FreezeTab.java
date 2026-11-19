package util.sail.call;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.PageLifecycle;

/**
 * Freezes a browser tab, or resumes it, as a browser does to a tab left in the background: a frozen page runs no script,
 * hears no message and answers nothing, but keeps its Web Locks. For a test of a tab that doesn't answer another tab
 * ({@link SwitchTab} opens the tabs). Freeze a tab that is not in front, and resume it before the leg ends: a driver
 * command to a frozen tab waits until it is resumed.
 * <ul>
 * <li>{@code action}: {@code freeze} or {@code resume} (required)</li>
 * <li>{@code index}: the tab, 0 being the first (the one opened first) (required)</li>
 * </ul>
 * No variable. Needs {@code PAGE_LIFECYCLE}.
 */
public final class FreezeTab extends AbstractSailCall {
	/** What the step does to the tab. */
	private enum Action {
		FREEZE, RESUME
	}

	private static final String ACTION = "action";

	/** Takes action and index. */
	public FreezeTab() {
		super(ACTION, "index");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		parameters.required(ACTION);
		Action action = parameters.choice(ACTION, Action.class, Action.FREEZE);
		int index = parameters.requiredInteger("index", 0);
		PageLifecycle lifecycle = require(context, PageLifecycle.class);
		List<String> handles = new ArrayList<>(context.driver().getWindowHandles());
		if (index >= handles.size()) {
			throw new IllegalStateException(String.format("%s can't %s tab %d (0 being the first): %d tab(s) are open",
															name(), action.name().toLowerCase(Locale.ROOT), Integer.valueOf(index), Integer.valueOf(handles.size())));
		}
		String handle = handles.get(index);
		if (action == Action.FREEZE) {
			lifecycle.freeze(handle);
		}
		else {
			lifecycle.resume(handle);
		}
		return null;
	}
}
