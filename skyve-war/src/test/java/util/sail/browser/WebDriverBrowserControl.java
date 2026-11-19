package util.sail.browser;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;

import org.openqa.selenium.Cookie;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.NoSuchWindowException;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WindowType;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.support.ui.WebDriverWait;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Controls any browser with plain WebDriver: its cookies, tabs, key presses and window. It supports
 * {@link Capability#COOKIES}, {@link Capability#TABS}, {@link Capability#KEYS} and {@link Capability#WINDOW}, and no
 * other capability, so a step that needs another is skipped.
 * <p>
 * Its cookies are those WebDriver gives: the cookies of the page in front's site, HttpOnly ones included.
 * <p>
 * Threading: thread-confined, as its driver is.
 */
public class WebDriverBrowserControl extends SailBrowserControl implements Cookies, Tabs, Keys, Window {
	private final WebDriver driver;

	/**
	 * Controls a browser through its driver; {@link SailBrowserControl#of(WebDriver)} makes it.
	 *
	 * @param driver	the browser's driver
	 */
	WebDriverBrowserControl(@Nonnull WebDriver driver) {
		super(browserName(driver));
		this.driver = driver;
	}

	/**
	 * Returns the browser's driver.
	 *
	 * @return the driver this control was made for
	 */
	protected final @Nonnull WebDriver driver() {
		return driver;
	}

	// ---- cookies

	@Override
	public @Nonnull List<BrowserCookie> cookies() {
		Set<Cookie> cookies = driver.manage().getCookies();
		List<BrowserCookie> result = new ArrayList<>(cookies.size());
		for (Cookie cookie : cookies) {
			String domain = cookie.getDomain();
			String path = cookie.getPath();
			result.add(new BrowserCookie(cookie.getName(),
											cookie.getValue(),
											(domain == null) ? "" : domain,
											(path == null) ? "" : path,
											cookie.getExpiry() == null));
		}
		return result;
	}

	/**
	 * {@inheritDoc}
	 *
	 * <p>Complexity: O(c) in the number of the site's cookies, and two driver commands.
	 */
	@Override
	public int deleteCookies(@Nonnull String name) {
		int result = 0;
		for (Cookie cookie : driver.manage().getCookies()) {
			if (name.equals(cookie.getName())) {
				result++;
			}
		}
		if (result > 0) {
			driver.manage().deleteCookieNamed(name);
		}
		return result;
	}

	// ---- tabs

	@Override
	public void openTab(@Nonnull String url) {
		driver.switchTo().newWindow(WindowType.TAB);
		driver.get(url);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @throws IllegalArgumentException	if the index is negative
	 */
	@Override
	public void switchToTab(int index, @Nonnull Duration timeout) {
		if (index < 0) {
			throw new IllegalArgumentException("A tab's index is 0 (the first) or more, not " + index);
		}
		// the event: the tab opens, as one a link with target _blank opens a moment after the click
		List<String> handles = await(timeout,
										d -> {
											List<String> open = new ArrayList<>(d.getWindowHandles());
											return (open.size() > index) ? open : null;
										},
										() -> String.format("Tab %d (0 being the first) did not open within %s: %d tab(s) are open",
															Integer.valueOf(index), timeout, Integer.valueOf(tabCount())));
		driver.switchTo().window(handles.get(index));
	}

	@Override
	public void closeTab() {
		driver.close();
		driver.switchTo().window(driver.getWindowHandles().iterator().next());
	}

	@Override
	public int tabCount() {
		return driver.getWindowHandles().size();
	}

	@Override
	public @Nullable <T> T visitNewTab(@Nonnull Duration timeout, boolean close, @Nonnull Supplier<T> visit) {
		String home = driver.getWindowHandle();
		// the event: the page opens another tab; the one opened last is visited
		String opened = await(timeout,
								d -> {
									List<String> others = new ArrayList<>(d.getWindowHandles());
									others.remove(home);
									return others.isEmpty() ? null : others.get(others.size() - 1);
								},
								() -> "No new tab opened within " + timeout);
		driver.switchTo().window(opened);
		try {
			return visit.get();
		}
		finally {
			if (close) {
				try {
					driver.close();
				}
				catch (@SuppressWarnings("unused") NoSuchWindowException e) {
					// the page closed the tab itself
				}
			}
			driver.switchTo().window(home);
		}
	}

	/**
	 * Waits for an event in the browser, as {@code Selenide.waitFor} does: a condition checked every 100 ms, ending as
	 * soon as it holds.
	 *
	 * @param <T>	the condition's result type
	 * @param timeout	how long to wait at most
	 * @param condition	the condition's result once the event has happened, null (or false) before
	 * @param failure	the message if it doesn't happen in time
	 * @return the condition's result
	 * @throws AssertionError	with the failure message, if the event doesn't happen in time
	 */
	private @Nonnull <T> T await(@Nonnull Duration timeout, @Nonnull Function<WebDriver, T> condition, @Nonnull Supplier<String> failure) {
		try {
			return new WebDriverWait(driver, timeout, POLLING).until(condition);
		}
		catch (TimeoutException e) {
			throw new AssertionError(failure.get(), e);
		}
	}

	// ---- keys

	/**
	 * {@inheritDoc}
	 *
	 * <p>The keys are one WebDriver action sequence, so nothing else happens between them.
	 */
	@Override
	public void press(@Nonnull CharSequence... keys) {
		int last = keys.length - 1;
		if (last < 0) {
			throw new IllegalArgumentException("Press needs at least one key");
		}
		Actions actions = new Actions(driver);
		for (int i = 0; i < last; i++) {
			actions.keyDown(keys[i]);
		}
		actions.sendKeys(keys[last]);
		for (int i = last - 1; i >= 0; i--) {
			actions.keyUp(keys[i]);
		}
		actions.perform();
	}

	// ---- window

	@Override
	public @Nonnull Dimension resizeWindow(int width, int height) {
		WebDriver.Window window = driver.manage().window();
		window.setSize(new Dimension(width, height));
		return window.getSize();
	}
}
