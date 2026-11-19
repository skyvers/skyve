package util.sail.call;

import java.time.Duration;

import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchSessionException;
import org.openqa.selenium.NoSuchWindowException;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.support.ui.WebDriverWait;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.SailBrowserControl;
import util.sail.browser.Tabs;

/**
 * Checks a tab the page opened (a link with {@code target="_blank"}) and comes back to the tab the step started in,
 * within the one step. In a SmartClient leg the executor waits for SmartClient to settle after every step, in whatever
 * tab is in front, so a step that only switched to a page without SmartClient would fail; this call visits the new tab
 * and returns before the step ends. The tab opened last is the one visited.
 * <ul>
 * <li>{@code script}: JavaScript run in the new tab, as the body of a function, that returns true once the tab shows
 * 		what it should (required)</li>
 * <li>{@code timeoutMillis}: how long to wait for the tab to appear, and then for {@code script} to hold; the leg's
 * 		timeout ({@code sail.timeoutSeconds}) by default</li>
 * <li>{@code close}: {@code false} to leave the new tab open; it is closed by default</li>
 * </ul>
 * Its variable, if the step names one, is the new tab's address. Needs {@code TABS}, which every browser has.
 */
public final class CheckOpenedTab extends AbstractSailCall {
	private static final String TIMEOUT_MILLIS = "timeoutMillis";

	/** Takes script, timeoutMillis and close. */
	public CheckOpenedTab() {
		super("script", TIMEOUT_MILLIS, "close");
	}

	@Override
	protected @Nullable String run(@Nonnull SailCallParameters parameters, @Nonnull Context context) {
		String script = parameters.required("script");
		Duration timeout = (parameters.optional(TIMEOUT_MILLIS) == null) ?
								context.timeout() :
								Duration.ofMillis(parameters.integer(TIMEOUT_MILLIS, 1, 1));
		boolean close = parameters.bool("close", true);
		Tabs tabs = require(context, Tabs.class);
		WebDriver driver = context.driver();
		return tabs.visitNewTab(timeout, close, () -> awaitScript(driver, script, timeout));
	}

	/**
	 * Waits for a script to return true in the tab in front: the event is the new tab loading and showing what it
	 * should. A run that throws (the page may still be loading) counts as false.
	 *
	 * @return the tab's address once the script holds
	 * @throws AssertionError	if it doesn't hold in time, with the script's last error
	 * @throws IllegalStateException	if the tab or the browser is gone
	 */
	private static @Nonnull String awaitScript(@Nonnull WebDriver driver, @Nonnull String script, @Nonnull Duration timeout) {
		String[] lastError = new String[1];
		try {
			return new WebDriverWait(driver, timeout, SailBrowserControl.POLLING).until(d -> {
				try {
					return Boolean.TRUE.equals(((JavascriptExecutor) d).executeScript(script)) ? d.getCurrentUrl() : null;
				}
				catch (NoSuchSessionException | NoSuchWindowException e) {
					// a wait ignores NotFoundException, which NoSuchWindowException is: fail at once instead
					throw new IllegalStateException("The new tab or the browser is gone: " + e.getMessage(), e);
				}
				catch (WebDriverException e) {
					lastError[0] = e.getMessage();
					return null;
				}
			});
		}
		catch (TimeoutException e) {
			throw new AssertionError("The new tab (" + driver.getCurrentUrl() + ") did not show what was expected within " + timeout + ": " + script +
										((lastError[0] == null) ? "" : " (the script last failed with: " + lastError[0] + ')'),
										e);
		}
	}
}
