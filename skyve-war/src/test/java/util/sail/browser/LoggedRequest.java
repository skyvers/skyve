package util.sail.browser;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A request that one of the browser's pages or workers made, as {@link RequestLog} saw it.
 *
 * @param method	the HTTP method
 * @param url	the URL
 * @param status	the response status, or null if there is no response (yet, or it failed)
 * @param failure	the network error's text, or null if it hasn't failed
 * @param fromServiceWorker	whether a service worker answered it for the page
 */
public record LoggedRequest(@Nonnull String method, @Nonnull String url, @Nullable Integer status, @Nullable String failure, boolean fromServiceWorker) {
}
