package util.sail.browser;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.function.Supplier;

import org.openqa.selenium.Dimension;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A browser control for unit tests of steps, with no browser: it implements every capability's interface, supports the
 * capabilities a test chooses, and records each operation called, in order, as text (such as {@code offline} or
 * {@code deleteCookies remember}). What the operations return comes from what the test gives it: the requests, dialogs
 * and cookies to report, how many requests are held, and how many tabs are open.
 * <p>
 * The operations act on that state as a browser would, simply: {@link #deleteCookies(String)} removes the cookies it
 * counts, {@link #releaseHeld()} leaves none held, {@link #openTab(String)} adds a tab, and so on. A wait that would not
 * end ({@link #awaitHeld(int, Duration)}, {@link #switchToTab(int, Duration)}, {@link #visitNewTab(Duration, boolean, Supplier)})
 * fails at once, as it would at its timeout.
 * <p>
 * Threading: not thread-safe.
 */
public class FakeBrowserControl extends SailBrowserControl
								implements RequestInterception, RequestLog, NetworkConditions, InitScripts, Dialogs, Cookies,
											StorageQuota, PageLifecycle, TouchEmulation, Tabs, Keys, Window {
	private final Set<Capability> supported;
	private final List<String> calls = new ArrayList<>();
	private final List<RequestRule> rules = new ArrayList<>();
	private final List<String> initScripts = new ArrayList<>();
	private final List<LoggedRequest> requests = new ArrayList<>();
	private final List<Dialog> dialogs = new ArrayList<>();
	private final List<BrowserCookie> cookies = new ArrayList<>();
	private int held;
	private int tabs = 1;

	/**
	 * Makes a fake of a browser that has some capabilities.
	 *
	 * @param browser	the browser's name, for the skip messages, such as {@code firefox}
	 * @param supported	the capabilities it has; none for a browser that skips every step that needs one
	 */
	public FakeBrowserControl(@Nonnull String browser, @Nonnull Capability... supported) {
		super(browser);
		this.supported = (supported.length == 0) ? EnumSet.noneOf(Capability.class) : EnumSet.copyOf(Arrays.asList(supported));
	}

	/**
	 * Makes a fake of a browser that has every capability.
	 *
	 * @return the fake, named {@code chrome}
	 */
	public static @Nonnull FakeBrowserControl supportingAll() {
		return new FakeBrowserControl("chrome", Capability.values());
	}

	/**
	 * Indicates whether the test chose this capability.
	 *
	 * @param capability	the capability
	 * @return true if it was given to the constructor
	 */
	@Override
	public boolean supports(@Nonnull Capability capability) {
		return supported.contains(capability);
	}

	/**
	 * Returns the operations called so far, in order, each as its name and its arguments separated by spaces.
	 *
	 * @return the calls; the live list, which a test may clear
	 */
	public @Nonnull List<String> calls() {
		return calls;
	}

	/**
	 * Returns the request rules added and not cleared.
	 *
	 * @return the rules, in the order added; the live list
	 */
	public @Nonnull List<RequestRule> rules() {
		return rules;
	}

	/**
	 * Returns the init scripts added.
	 *
	 * @return the scripts, without repeats, in the order added; the live list
	 */
	public @Nonnull List<String> initScripts() {
		return initScripts;
	}

	/**
	 * Adds requests to the log that {@link #requests()} reports.
	 *
	 * @param logged	the requests, in the order they started
	 * @return this fake
	 */
	public @Nonnull FakeBrowserControl withRequests(@Nonnull LoggedRequest... logged) {
		requests.addAll(Arrays.asList(logged));
		return this;
	}

	/**
	 * Adds dialogs to those that {@link #dialogs()} reports.
	 *
	 * @param seen	the dialogs, oldest first
	 * @return this fake
	 */
	public @Nonnull FakeBrowserControl withDialogs(@Nonnull Dialog... seen) {
		dialogs.addAll(Arrays.asList(seen));
		return this;
	}

	/**
	 * Adds cookies to those that {@link #cookies()} reports.
	 *
	 * @param given	the cookies
	 * @return this fake
	 */
	public @Nonnull FakeBrowserControl withCookies(@Nonnull BrowserCookie... given) {
		cookies.addAll(Arrays.asList(given));
		return this;
	}

	/**
	 * Sets how many requests are held.
	 *
	 * @param count	the number
	 * @return this fake
	 */
	public @Nonnull FakeBrowserControl withHeld(int count) {
		held = count;
		return this;
	}

	/**
	 * Sets how many tabs are open (1 to start with).
	 *
	 * @param count	the number
	 * @return this fake
	 */
	public @Nonnull FakeBrowserControl withTabs(int count) {
		tabs = count;
		return this;
	}

	private void record(@Nonnull Object... parts) {
		StringBuilder call = new StringBuilder(64);
		for (Object part : parts) {
			if (call.length() > 0) {
				call.append(' ');
			}
			call.append(part);
		}
		calls.add(call.toString());
	}

	// ---- request interception

	@Override
	public void addRule(@Nonnull RequestRule rule) {
		record("addRule", rule);
		rules.add(rule);
	}

	@Override
	public void clearRules() {
		record("clearRules");
		rules.clear();
		held = 0;
	}

	@Override
	public int heldCount() {
		record("heldCount");
		return held;
	}

	/**
	 * Records the wait, and fails at once if fewer than {@code count} requests are held.
	 */
	@Override
	public void awaitHeld(int count, @Nonnull Duration timeout) {
		record("awaitHeld", Integer.valueOf(count), timeout);
		if (held < count) {
			throw new AssertionError(String.format("Expected %d held requests within %s, but %d were held",
													Integer.valueOf(count), timeout, Integer.valueOf(held)));
		}
	}

	@Override
	public void releaseHeld() {
		record("releaseHeld");
		held = 0;
	}

	@Override
	public void failHeld(@Nonnull RequestRule.ErrorReason reason) {
		record("failHeld", reason);
		held = 0;
	}

	// ---- request log

	@Override
	public @Nonnull List<LoggedRequest> requests() {
		record("requests");
		return new ArrayList<>(requests);
	}

	@Override
	public void clearRequests() {
		record("clearRequests");
		requests.clear();
	}

	// ---- network

	@Override
	public void offline() {
		record("offline");
	}

	@Override
	public void online() {
		record("online");
	}

	@Override
	public void throttle(@Nonnull Duration latency, long downloadBytesPerSecond, long uploadBytesPerSecond) {
		record("throttle", latency, Long.valueOf(downloadBytesPerSecond), Long.valueOf(uploadBytesPerSecond));
	}

	// ---- init scripts

	@Override
	public void addInitScript(@Nonnull String source) {
		record("addInitScript", source);
		if (! initScripts.contains(source)) {
			initScripts.add(source);
		}
	}

	// ---- dialogs

	@Override
	public @Nonnull List<Dialog> dialogs() {
		record("dialogs");
		return new ArrayList<>(dialogs);
	}

	@Override
	public void clearDialogs() {
		record("clearDialogs");
		dialogs.clear();
	}

	@Override
	public void answerDialog(boolean accept, @Nullable String promptText) {
		record("answerDialog", Boolean.valueOf(accept), promptText);
	}

	// ---- cookies

	@Override
	public @Nonnull List<BrowserCookie> cookies() {
		record("cookies");
		return new ArrayList<>(cookies);
	}

	/**
	 * Records the deletion, and removes the cookies with the name.
	 */
	@Override
	public int deleteCookies(@Nonnull String name) {
		record("deleteCookies", name);
		int before = cookies.size();
		cookies.removeIf(c -> c.name().equals(name));
		return before - cookies.size();
	}

	// ---- storage quota

	@Override
	public void overrideQuota(@Nonnull String origin, long bytes) {
		record("overrideQuota", origin, Long.valueOf(bytes));
	}

	@Override
	public void clearQuotaOverride(@Nonnull String origin) {
		record("clearQuotaOverride", origin);
	}

	// ---- page lifecycle

	@Override
	public void freeze(@Nonnull String windowHandle) {
		record("freeze", windowHandle);
	}

	@Override
	public void resume(@Nonnull String windowHandle) {
		record("resume", windowHandle);
	}

	// ---- touch

	@Override
	public void emulateTouch(int maxTouchPoints) {
		record("emulateTouch", Integer.valueOf(maxTouchPoints));
	}

	// ---- tabs

	@Override
	public void openTab(@Nonnull String url) {
		record("openTab", url);
		tabs++;
	}

	/**
	 * Records the switch, and fails at once if there aren't {@code index + 1} tabs.
	 */
	@Override
	public void switchToTab(int index, @Nonnull Duration timeout) {
		record("switchToTab", Integer.valueOf(index), timeout);
		if (index >= tabs) {
			throw new AssertionError(String.format("Tab %d (0 being the first) did not open within %s: %d tab(s) are open",
													Integer.valueOf(index), timeout, Integer.valueOf(tabs)));
		}
	}

	@Override
	public void closeTab() {
		record("closeTab");
		tabs--;
	}

	@Override
	public int tabCount() {
		record("tabCount");
		return tabs;
	}

	/**
	 * Records the visit, fails at once if only one tab is open, and otherwise does the visit, closing the tab
	 * afterwards if asked.
	 */
	@Override
	public @Nullable <T> T visitNewTab(@Nonnull Duration timeout, boolean close, @Nonnull Supplier<T> visit) {
		record("visitNewTab", timeout, Boolean.valueOf(close));
		if (tabs < 2) {
			throw new AssertionError("No new tab opened within " + timeout);
		}
		try {
			return visit.get();
		}
		finally {
			if (close) {
				tabs--;
			}
		}
	}

	// ---- keys

	@Override
	public void press(@Nonnull CharSequence... keys) {
		if (keys.length == 0) {
			throw new IllegalArgumentException("Press needs at least one key");
		}
		List<String> names = new ArrayList<>(keys.length);
		for (CharSequence key : keys) {
			names.add((key instanceof org.openqa.selenium.Keys k) ? k.name() : key.toString());
		}
		record("press", String.join("+", names));
	}

	// ---- window

	@Override
	public @Nonnull Dimension resizeWindow(int width, int height) {
		record("resizeWindow", Integer.valueOf(width), Integer.valueOf(height));
		return new Dimension(width, height);
	}
}
