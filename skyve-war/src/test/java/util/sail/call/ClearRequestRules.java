package util.sail.call;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.RequestInterception;

/**
 * Removes every request rule ({@link FailRequests}, {@link HoldRequests}, {@link RespondToRequests},
 * {@link LoseResponses}, {@link RewriteRequestHeaders}), letting held requests carry on first.
 * <p>
 * No parameters, and no variable. Needs {@code REQUEST_INTERCEPTION}.
 */
public final class ClearRequestRules extends AbstractSailCall {
	/** Takes no parameters. */
	public ClearRequestRules() {
		super();
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		require(context, RequestInterception.class).clearRules();
		return null;
	}
}
