package util.sail.browser;

import jakarta.annotation.Nonnull;

/**
 * What a test can ask a browser to do beyond plain WebDriver, one group of operations each. Each capability has the
 * interface that offers its operations ({@link #type()}); {@link SailBrowserControl#supports(Capability)} says whether
 * the current browser has it, and {@link SailBrowserControl#require(Class, String)} returns its interface or skips
 * the test.
 */
public enum Capability {
	/** Rules that fail, hold, answer or change requests, or lose their responses: {@link RequestInterception}. */
	REQUEST_INTERCEPTION(RequestInterception.class),
	/** The requests the pages and workers made: {@link RequestLog}. */
	REQUEST_LOG(RequestLog.class),
	/** Offline, online and a slow network, for pages and workers: {@link NetworkConditions}. */
	NETWORK_CONDITIONS(NetworkConditions.class),
	/** Scripts run before a page's own in every new document: {@link InitScripts}. */
	INIT_SCRIPTS(InitScripts.class),
	/** The JavaScript dialogs pages opened, and answering them: {@link Dialogs}. */
	DIALOGS(Dialogs.class),
	/** Listing and deleting cookies, HttpOnly ones included: {@link Cookies}. */
	COOKIES(Cookies.class),
	/** An origin's storage quota: {@link StorageQuota}. */
	STORAGE_QUOTA(StorageQuota.class),
	/** Freezing and resuming a tab: {@link PageLifecycle}. */
	PAGE_LIFECYCLE(PageLifecycle.class),
	/** A touch screen: {@link TouchEmulation}. */
	TOUCH(TouchEmulation.class),
	/** Opening, switching, closing and visiting tabs: {@link Tabs}. */
	TABS(Tabs.class),
	/** Trusted key presses and chords: {@link Keys}. */
	KEYS(Keys.class),
	/** The window's size: {@link Window}. */
	WINDOW(Window.class);

	private final Class<?> type;

	Capability(@Nonnull Class<?> type) {
		this.type = type;
	}

	/**
	 * Returns the interface that offers this capability's operations.
	 *
	 * @return the interface; each capability has its own
	 */
	public @Nonnull Class<?> type() {
		return type;
	}

	/**
	 * Returns the capability whose operations an interface offers.
	 *
	 * <p>Complexity: O(c) in the number of capabilities (twelve).
	 *
	 * @param type	one of the capabilities' interfaces, such as {@link Cookies}
	 * @return its capability
	 * @throws IllegalArgumentException	if the type is not a capability's interface
	 */
	public static @Nonnull Capability of(@Nonnull Class<?> type) {
		for (Capability capability : values()) {
			if (capability.type == type) {
				return capability;
			}
		}
		throw new IllegalArgumentException(type.getName() + " is not the interface of a browser capability");
	}
}
