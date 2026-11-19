package util.sail.call;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.Cookies;

/**
 * Deletes cookies from the leg's browser, HttpOnly ones included, as if they had expired: {@code JSESSIONID} for a
 * session that has ended, with the remember-me cookie for one that can't be signed in again. Plain WebDriver sees only
 * the cookies of the site of the page in front, so open a page of the site first.
 * <ul>
 * <li>{@code names}: the cookie names, comma separated (required)</li>
 * </ul>
 * Its variable, if the step names one, is how many cookies it deleted. Needs {@code COOKIES}, which every browser has.
 */
public final class DeleteCookies extends AbstractSailCall {
	private static final String NAMES = "names";

	/** Takes names. */
	public DeleteCookies() {
		super(NAMES);
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		parameters.required(NAMES);
		Cookies cookies = require(context, Cookies.class);
		int deleted = 0;
		for (String name : parameters.list(NAMES)) {
			deleted += cookies.deleteCookies(name);
		}
		return String.valueOf(deleted);
	}
}
