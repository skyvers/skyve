package util.sail.call;

import java.util.List;
import java.util.Locale;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.Dialog;
import util.sail.browser.Dialogs;

/**
 * Checks that a page opened a JavaScript dialog since the last check (or since the browser's control began watching),
 * then forgets the dialogs seen. The control begins watching the first time a step asks for it, so a check in the step
 * where watching began fails, as it would know nothing of the steps before: put a call before the steps to watch, such
 * as {@code RequestLog} with {@code clear=true}. ChromeDriver accepts a {@code beforeunload} prompt at once, so this
 * record is the only way a script sees one; "Stay on the page" can't be tested. Chrome asks a {@code beforeunload}
 * question only once the user has used the page, so press a key or click first.
 * <ul>
 * <li>{@code type}: {@code beforeunload} (the default), {@code alert}, {@code confirm} or {@code prompt}</li>
 * <li>{@code expected}: {@code false} to check that no such dialog opened; {@code true} by default</li>
 * </ul>
 * No variable. Needs {@code DIALOGS}.
 */
public final class CheckDialog extends AbstractSailCall {
	/** Takes type and expected. */
	public CheckDialog() {
		super("type", "expected");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		Dialog.Type wanted = parameters.choice("type", Dialog.Type.class, Dialog.Type.BEFOREUNLOAD);
		boolean expected = parameters.bool("expected", true);
		Dialogs seen = require(context, Dialogs.class);
		List<Dialog> dialogs = seen.dialogs();
		requireWatchingBefore(context, seen.watchingSince(), "record of dialogs");
		seen.clearDialogs();
		boolean opened = false;
		for (Dialog dialog : dialogs) {
			if (dialog.type() == wanted) {
				opened = true;
				break;
			}
		}
		if (opened != expected) {
			throw new AssertionError(String.format("Expected %s %s dialog, but the dialogs were %s",
													expected ? "a" : "no",
													wanted.name().toLowerCase(Locale.ROOT),
													dialogs));
		}
		return null;
	}
}
