package util.sail.call;

import java.time.Duration;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.NetworkConditions;

/**
 * Slows the leg's whole browser (its pages and every worker) to a weak signal, until {@link Online}.
 * <ul>
 * <li>{@code latencyMillis}: the round trip time added, 0 or more (required)</li>
 * <li>{@code downKbps}, {@code upKbps}: the speeds in kilobits a second; unlimited (-1) by default</li>
 * </ul>
 * No variable. Needs {@code NETWORK_CONDITIONS}.
 */
public final class Throttle extends AbstractSailCall {
	/** Takes latencyMillis, downKbps and upKbps. */
	public Throttle() {
		super("latencyMillis", "downKbps", "upKbps");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		int latency = parameters.requiredInteger("latencyMillis", 0);
		long down = bytesPerSecond(parameters.integer("downKbps", -1, -1));
		long up = bytesPerSecond(parameters.integer("upKbps", -1, -1));
		require(context, NetworkConditions.class).throttle(Duration.ofMillis(latency), down, up);
		return null;
	}

	/** Converts kilobits a second to bytes a second, keeping -1 (unlimited). */
	private static long bytesPerSecond(int kilobits) {
		return (kilobits < 0) ? -1L : (kilobits * 1000L / 8L);
	}
}
