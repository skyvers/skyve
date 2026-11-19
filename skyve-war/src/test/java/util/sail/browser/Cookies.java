package util.sail.browser;

import java.util.List;

import jakarta.annotation.Nonnull;

/**
 * The browser's cookies, HttpOnly ones included, which a page script can't see or delete.
 * <p>
 * The capability {@link Capability#COOKIES}.
 */
public interface Cookies {
	/**
	 * Returns the cookies of the site of the page in front, HttpOnly ones included. An implementation may return other
	 * sites' cookies too.
	 *
	 * @return the cookies; a copy the caller owns
	 */
	@Nonnull List<BrowserCookie> cookies();

	/**
	 * Deletes the cookies with a name that {@link #cookies()} lists, HttpOnly ones included, as if they had expired.
	 *
	 * @param name	the cookie name
	 * @return how many were deleted
	 */
	int deleteCookies(@Nonnull String name);
}
