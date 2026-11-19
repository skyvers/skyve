package util.sail.call;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.InitScripts;

/**
 * Makes IndexedDB unavailable to every page the browser loads from now on, as in a browser that refuses it: a script
 * run before the page's own makes {@code window.indexedDB} undefined. Call it before navigating to the page under test.
 * Nothing puts it back while the browser runs.
 * <p>
 * No parameters, and no variable. Needs {@code INIT_SCRIPTS}.
 */
public final class BlockIndexedDb extends AbstractSailCall {
	/** Takes no parameters. */
	public BlockIndexedDb() {
		super();
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		require(context, InitScripts.class).addInitScript("Object.defineProperty(window, 'indexedDB', { value: undefined, configurable: true });");
		return null;
	}
}
