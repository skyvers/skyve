package util.sail.call;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.NetworkConditions;

/**
 * Brings the leg's whole browser back online at full speed, after {@link Offline} or {@link Throttle}.
 * <p>
 * No parameters, and no variable. Needs {@code NETWORK_CONDITIONS}.
 */
public final class Online extends AbstractSailCall {
	/** Takes no parameters. */
	public Online() {
		super();
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		require(context, NetworkConditions.class).online();
		return null;
	}
}
