package util.sail.call;

import java.util.concurrent.atomic.AtomicLong;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.InitScripts;

/**
 * Sets the page's clock ahead (or behind) in every document the browser loads from now on, before the page's own
 * scripts: {@code new Date()} and {@code Date.now()} give the real time plus an offset. The server's clock is not
 * touched. It takes effect from the next page load, and lasts while the browser runs; a second shift adds to the first.
 * <ul>
 * <li>{@code days}: how many days ahead; negative for behind, down to -3650 (required)</li>
 * </ul>
 * No variable. Needs {@code INIT_SCRIPTS}.
 */
public final class ShiftClock extends AbstractSailCall {
	/** Numbers each shift, so a repeated shift is a script of its own: the control adds the same script only once. */
	private static final AtomicLong SHIFTS = new AtomicLong();

	/** Takes days. */
	public ShiftClock() {
		super("days");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		long millis = parameters.requiredInteger("days", -3650) * 86_400_000L;
		// each shift wraps the Date the shifts before it left, so they add up
		require(context, InitScripts.class).addInitScript("""
				// SAIL ShiftClock %d
				(() => {
				const offset = %d;
				const Real = Date;
				class Shifted extends Real {
				constructor(...args) { if (args.length === 0) { super(Real.now() + offset); } else { super(...args); } }
				static now() { return Real.now() + offset; }
				}
				window.Date = Shifted;
				})();
				""".formatted(Long.valueOf(SHIFTS.incrementAndGet()), Long.valueOf(millis)));
		return null;
	}
}
