package util.sail.call;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.Tabs;

/**
 * Works the leg's browser tabs, which share its profile (cookies, storage and service workers), for tests of several
 * tabs. The executor's contexts stay with the tab the automation started in, so work in another tab with script steps
 * and calls only, and come back before the next gesture. To look at a tab the page opens and come back in one step, use
 * {@link CheckOpenedTab}.
 * <ul>
 * <li>{@code action}: {@code open} (a new tab, in front, on {@code url}), {@code switch} (to the tab at {@code index})
 * 		or {@code close} (the tab in front; the first tab comes to the front) (required)</li>
 * <li>{@code url}: for {@code open}, relative to the base URL, or absolute</li>
 * <li>{@code index}: for {@code switch}, 0 being the first tab (the one opened first). A tab that the page opens (a link
 * 		with {@code target="_blank"}) can take a moment to appear, so it is waited for, up to the leg's timeout
 * 		({@code sail.timeoutSeconds})</li>
 * </ul>
 * Its variable, if the step names one, is the number of tabs open afterwards. Needs {@code TABS}, which every browser
 * has.
 */
public final class SwitchTab extends AbstractSailCall {
	/** Takes action, url and index. */
	public SwitchTab() {
		super("action", "url", "index");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		String action = parameters.required("action").trim();
		Tabs tabs;
		switch (action) {
			case "open" -> {
				String url = parameters.required("url").trim();
				tabs = require(context, Tabs.class);
				tabs.openTab(absolute(url, context));
			}
			case "switch" -> {
				int index = parameters.requiredInteger("index", 0);
				tabs = require(context, Tabs.class);
				tabs.switchToTab(index, context.timeout());
			}
			case "close" -> {
				tabs = require(context, Tabs.class);
				tabs.closeTab();
			}
			default -> throw new IllegalArgumentException(name() + "'s action must be open, switch or close, not " + action);
		}
		return String.valueOf(tabs.tabCount());
	}

	/**
	 * Returns a URL as given if it has a scheme, or else relative to the leg's base URL, as {@code <navigateLink/>} takes
	 * an {@code href}.
	 */
	private static @Nonnull String absolute(@Nonnull String url, @Nonnull Context context) {
		if (url.matches("^[a-zA-Z][a-zA-Z0-9+.-]*:.*")) {
			return url;
		}
		return context.baseUrl() + (url.startsWith("/") ? url.substring(1) : url);
	}
}
