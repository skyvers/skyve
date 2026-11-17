package util.sail;

import java.util.function.LongSupplier;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * The iteration of a load run's virtual user ({@link SailLoad}) that is running on this thread, for the harness to
 * report to: the executor times each interaction with it ({@link InterpretedWebDriverExecutor#executeInteraction}),
 * and the failure screenshots and media folders are named for the user and the iteration, as the virtual users share
 * their directories. Outside a load run no probe is set, and the harness does what it always did.
 */
final class SailLoadProbe {
	/**
	 * Receives what the interactions of an iteration did.
	 */
	@FunctionalInterface
	interface Listener {
		/**
		 * Notes an interaction that ended.
		 *
		 * @param name	the interaction's name
		 * @param startNanos	when it started, from the probe's clock
		 * @param endNanos	when it ended, from the probe's clock
		 * @param failed	true if a step threw or a check failed
		 * @param failure	what failed, or null if it passed or the reason is not known
		 */
		void interaction(@Nonnull String name, long startNanos, long endNanos, boolean failed, @Nullable String failure);
	}

	private static final ThreadLocal<SailLoadProbe> CURRENT = new ThreadLocal<>();

	private final String label;
	private final LongSupplier clock;
	private final Listener listener;

	/**
	 * Creates a probe for one iteration.
	 *
	 * @param label	names the user and iteration in file names, e.g. {@code user03-it002}
	 * @param clock	the load run's clock, in nanoseconds
	 * @param listener	receives the interactions
	 */
	SailLoadProbe(@Nonnull String label, @Nonnull LongSupplier clock, @Nonnull Listener listener) {
		this.label = label;
		this.clock = clock;
		this.listener = listener;
	}

	/**
	 * Returns the probe of the iteration running on this thread.
	 *
	 * @return the probe, or null outside a load run
	 */
	static @Nullable SailLoadProbe current() {
		return CURRENT.get();
	}

	/**
	 * Sets, or with null clears, the probe of the iteration running on this thread.
	 *
	 * <p>Side effects: sets this thread's probe, which the caller clears in a {@code finally}.
	 *
	 * @param probe	the iteration's probe, or null
	 */
	static void set(@Nullable SailLoadProbe probe) {
		if (probe == null) {
			CURRENT.remove();
		}
		else {
			CURRENT.set(probe);
		}
	}

	/**
	 * Returns what starts the names of this thread's evidence files and folders, so that virtual users that share a
	 * directory do not overwrite each other's.
	 *
	 * @return the user and iteration and a {@code -}, e.g. {@code user03-it002-}; empty outside a load run
	 */
	static @Nonnull String fileLabel() {
		SailLoadProbe probe = CURRENT.get();
		return (probe == null) ? "" : probe.label + '-';
	}

	/**
	 * Returns the time now, from the load run's clock.
	 *
	 * @return nanoseconds, for differences only
	 */
	long now() {
		return clock.getAsLong();
	}

	/**
	 * Notes an interaction that has just ended.
	 *
	 * @param name	the interaction's name
	 * @param startNanos	when it started, from {@link #now()}
	 * @param failed	true if a step threw or a check failed
	 * @param failure	what failed, or null if it passed or the reason is not known
	 */
	void interaction(@Nonnull String name, long startNanos, boolean failed, @Nullable String failure) {
		listener.interaction(name, startNanos, clock.getAsLong(), failed, failure);
	}
}
