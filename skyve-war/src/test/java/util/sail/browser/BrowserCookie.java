package util.sail.browser;

import jakarta.annotation.Nonnull;

/**
 * A cookie in the browser, as {@link Cookies} lists it.
 *
 * @param name	the name
 * @param value	the value
 * @param domain	the domain
 * @param path	the path
 * @param session	whether it is a session cookie, lost when the browser stops
 */
public record BrowserCookie(@Nonnull String name, @Nonnull String value, @Nonnull String domain, @Nonnull String path, boolean session) {
}
