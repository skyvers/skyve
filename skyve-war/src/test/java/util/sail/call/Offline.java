package util.sail.call;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.NetworkConditions;

/**
 * Takes the leg's whole browser offline: its pages and every service, shared and dedicated worker, now and as they
 * start, so a page's service worker can't reach the network either. Until {@link Online}.
 * <p>
 * No parameters, and no variable. Needs {@code NETWORK_CONDITIONS}.
 */
public final class Offline extends AbstractSailCall {
	/** Takes no parameters. */
	public Offline() {
		super();
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		require(context, NetworkConditions.class).offline();
		return null;
	}
}
