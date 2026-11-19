package util.sail.call;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.RequestInterception;
import util.sail.browser.RequestRule.ErrorReason;

/**
 * Lets the held requests (see {@link HoldRequests}) carry on to the server, or fails them. A request the page has given
 * up on in the meantime is skipped. The rules stay in place: later matching requests are held too, until
 * {@link ClearRequestRules}.
 * <ul>
 * <li>{@code fail}: a network error to fail them with instead, such as {@code TimedOut} (any {@link ErrorReason});
 * 		they carry on by default</li>
 * </ul>
 * No variable. Needs {@code REQUEST_INTERCEPTION}.
 */
public final class ReleaseRequests extends AbstractSailCall {
	/** Takes fail. */
	public ReleaseRequests() {
		super("fail");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		ErrorReason reason = (parameters.optional("fail") == null) ? null : parameters.choice("fail", ErrorReason.class, ErrorReason.FAILED);
		RequestInterception interception = require(context, RequestInterception.class);
		if (reason == null) {
			interception.releaseHeld();
		}
		else {
			interception.failHeld(reason);
		}
		return null;
	}
}
