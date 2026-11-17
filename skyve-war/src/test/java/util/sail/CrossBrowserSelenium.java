package util.sail;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.openqa.selenium.Dimension;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.Options;
import org.openqa.selenium.WebDriver.Timeouts;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.edge.EdgeDriver;
import org.openqa.selenium.edge.EdgeOptions;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.firefox.FirefoxOptions;
import org.openqa.selenium.firefox.FirefoxProfile;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.safari.SafariDriver;
import org.openqa.selenium.safari.SafariOptions;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

abstract class CrossBrowserSelenium {
	protected WebDriver driver;
	protected BrowserConfiguration configuration;
	protected Actions actions;
	/** The settings the browser was started with, resolved from its configuration; null before it starts. */
	protected SailSettings settings;

	/** The user agent the browser was started with, to tell when a configuration asks for another; or null. */
	private String startedUserAgent;
	/** The window size the browser was started with, to tell when a configuration asks for another; or null. */
	private Dimension startedResolution;

	/**
	 * Starts the browser that the configuration's resolved settings name, with their headless mode.
	 *
	 * @param configuration	the browser's user agent, window size and timeouts, and its own settings
	 * @throws IllegalArgumentException	if a setting is invalid (before any browser starts)
	 */
	public void startBrowser(@SuppressWarnings("hiding") BrowserConfiguration configuration) {
		startBrowser(configuration, configuration.resolve());
	}

	/**
	 * Starts the browser with settings already resolved from the configuration.
	 *
	 * @param configuration	the browser's user agent, window size and timeouts
	 * @param resolved	the settings resolved from it, which name the browser, its headless mode and the timeouts
	 */
	public void startBrowser(@SuppressWarnings("hiding") BrowserConfiguration configuration, @Nonnull SailSettings resolved) {
		this.configuration = configuration;
		this.settings = resolved;
		startedUserAgent = configuration.getUserAgentString();
		startedResolution = configuration.getResolution();
		switch (resolved.browser()) {
			case chrome:
				setupChrome(configuration, resolved);
				break;
			case firefox:
				setupFirefox(configuration, resolved);
				break;
			case safari:
				setupSafari(configuration, resolved);
				break;
			case edge:
				setupEdge(configuration, resolved);
				break;
			default:
				throw new IllegalStateException("Browser not supported");
		}
		actions = new Actions(driver);
	}

	/**
	 * Says how the browser that started differs from the one that settings and a configuration ask for: another
	 * browser, headless mode, server, user agent or window size.
	 *
	 * @param resolved	the settings the next script runs with
	 * @param next	the configuration the next script runs with
	 * @return what differs, e.g. {@code headless false -> true}; or null if the same browser will do (or none started)
	 */
	@Nullable String browserChange(@Nonnull SailSettings resolved, @Nonnull BrowserConfiguration next) {
		SailSettings started = settings;
		if (started == null) {
			return null;
		}
		List<String> changes = new ArrayList<>(5);
		change(changes, "browser", started.browser(), resolved.browser());
		change(changes, "headless", Boolean.valueOf(started.headless()), Boolean.valueOf(resolved.headless()));
		change(changes, "baseUrl", started.baseUrl(), resolved.baseUrl());
		change(changes, "user agent", startedUserAgent, next.getUserAgentString());
		change(changes, "resolution", startedResolution, next.getResolution());
		return changes.isEmpty() ? null : String.join(", ", changes);
	}

	private static void change(@Nonnull List<String> changes, @Nonnull String what, @Nullable Object from, @Nullable Object to) {
		if (! Objects.equals(from, to)) {
			changes.add(what + ' ' + from + " -> " + to);
		}
	}

	/**
	 * Drives the browser that another instance started, instead of starting one: for example a SmartClient executor's
	 * gestures in the browser that a PrimeFaces script used, when a workflow moves to SmartClient on the same device.
	 * The browser stays the other instance's too; whichever is used last stops it.
	 *
	 * @param other	the instance whose browser to drive
	 */
	public void useBrowserOf(@Nonnull CrossBrowserSelenium other) {
		this.configuration = other.configuration;
		this.settings = other.settings;
		this.startedUserAgent = other.startedUserAgent;
		this.startedResolution = other.startedResolution;
		this.driver = other.driver;
		this.actions = other.actions;
	}

	/**
	 * Returns the settings the browser was started with.
	 *
	 * @return the settings, or null before the browser starts
	 */
	public @Nullable SailSettings getSettings() {
		return settings;
	}

	/**
	 * Returns the configuration the browser was started with.
	 *
	 * @return the configuration, or null before the browser starts
	 */
	public @Nullable BrowserConfiguration getConfiguration() {
		return configuration;
	}

	/**
	 * Quits the browser, if one started. A thread that was interrupted (a load run's user stopped at its time limit)
	 * still quits it: the interrupt is set aside while the browser quits, as the driver's requests would fail at once
	 * and leave the browser running, and then put back. So an interrupted thread can block here for up to the driver's
	 * command timeout, if the browser does not answer.
	 */
	public void stopBrowser() {
		if (driver != null) {
			boolean interrupted = Thread.interrupted();
			try {
				driver.quit();
			}
			finally {
				if (interrupted) {
					Thread.currentThread().interrupt();
				}
			}
		}
	}

	/**
	 * Starts Chrome with the configuration's user agent and window size.
	 *
	 * @param config	the user agent, window size and timeouts
	 * @param resolved	the settings: headless mode and the timeout
	 */
	private void setupChrome(BrowserConfiguration config, SailSettings resolved) {
		List<String> arguments = new ArrayList<>(6);
		// Chrome throttles windows it thinks are hidden (a locked screen, behind other apps): keep timers and rendering running
		arguments.add("--disable-backgrounding-occluded-windows");
		arguments.add("--disable-renderer-backgrounding");
		arguments.add("--disable-background-timer-throttling");
		String userAgentString = config.getUserAgentString();
		if (userAgentString != null) {
			arguments.add("--user-agent=" + userAgentString);
		}
		if (resolved.headless()) {
			arguments.add("--headless");
			// Headless Chrome opens at 800x600, which squeezes the layouts; size it like a desktop window.
			if (config.getResolution() == null) {
				arguments.add("--window-size=1920,1080");
			}
		}

		ChromeOptions options = new ChromeOptions();
		options.addArguments(arguments);
		driver = new ChromeDriver(options);
		
		setupBrowser(config, resolved);
	}

	/**
	 * Starts Firefox with the configuration's window size.
	 *
	 * @param config	the user agent, window size and timeouts
	 * @param resolved	the settings: headless mode and the timeout
	 */
	private void setupFirefox(BrowserConfiguration config, SailSettings resolved) {
		FirefoxOptions options = new FirefoxOptions();
		if (resolved.headless()) {
			options.addArguments("-headless");
		}
		FirefoxProfile profile = new FirefoxProfile();
		String userAgentString = config.getUserAgentString();
		if (userAgentString != null) {
			profile.setPreference("general.useragent.override", "USER AGENT STRING");
		}
		options.setProfile(profile);
		driver = new FirefoxDriver(options);

		setupBrowser(config, resolved);
	}
	
	/**
	 * run "safaridriver --enable" in a terminal.
	 * Safari does not have a headless option.
	 * 
	 * @param config	the user agent, window size and timeouts
	 * @param resolved	the settings: the timeout
	 */
	private void setupSafari(BrowserConfiguration config, SailSettings resolved) {
		SafariOptions options = new SafariOptions();
		driver = new SafariDriver(options);
		
		setupBrowser(config, resolved);
	}

	private void setupEdge(BrowserConfiguration config, SailSettings resolved) {
		List<String> arguments = new ArrayList<>(2);
		String userAgentString = config.getUserAgentString();
		if (userAgentString != null) {
			arguments.add("--user-agent=" + userAgentString);
		}
		if (resolved.headless()) {
			arguments.add("headless");
		}
		
		EdgeOptions options = new EdgeOptions();
		options.addArguments(arguments);
		driver = new EdgeDriver(options);
		
		setupBrowser(config, resolved);
	}
	
	private void setupBrowser(BrowserConfiguration config, SailSettings resolved) {
		Options manage = driver.manage();
		
		Timeouts timeouts = manage.timeouts();
		Duration duration = config.getPageLoadTimeout();
		timeouts.pageLoadTimeout((duration == null) ? resolved.timeout() : duration);
		duration = config.getImplicitlyWait();
		timeouts.implicitlyWait((duration == null) ? Duration.ofMillis(0) : duration);
		duration = config.getScriptTimeout();
		if (duration != null) {
			timeouts.scriptTimeout(duration);
		}
		
		Dimension resolution = config.getResolution();
		if (resolution != null) {
			manage.window().setSize(resolution);
		}
	}
}
