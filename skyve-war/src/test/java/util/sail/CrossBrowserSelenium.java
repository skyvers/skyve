package util.sail;

import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.openqa.selenium.By;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.Options;
import org.openqa.selenium.WebDriver.Timeouts;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.chromium.ChromiumOptions;
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
	/** The profile directory the browser was started with, to tell when a configuration asks for another; or null. */
	private Path startedProfile;
	/** The extra arguments the browser was started with, to tell when a configuration asks for others. */
	private List<String> startedArguments = List.of();

	/**
	 * Starts the browser that the configuration's resolved settings name, with their headless mode.
	 *
	 * @param configuration	the browser's user agent, window size and timeouts, and its own settings
	 * @throws IllegalArgumentException	if a setting is invalid, or the configuration has a profile or extra arguments
	 * 									and the browser is not Chrome or Edge (before any browser starts)
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
		requireProfileSupport(resolved.browser(), configuration);
		recordStart(configuration, resolved);
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
	 * Records what the browser starts with, to tell later when a configuration asks for another browser.
	 *
	 * @param config	the configuration the browser starts with
	 * @param resolved	the settings resolved from it
	 */
	void recordStart(@Nonnull BrowserConfiguration config, @Nonnull SailSettings resolved) {
		this.configuration = config;
		this.settings = resolved;
		startedUserAgent = config.getUserAgentString();
		startedResolution = config.getResolution();
		startedProfile = config.getProfile();
		// a snapshot: the configuration's own list can still grow
		startedArguments = List.copyOf(config.getArguments());
	}

	/**
	 * Says how the browser that started differs from the one that settings and a configuration ask for: another
	 * browser, headless mode, server, user agent or window size.
	 *
	 * @param resolved	the settings the next script runs with
	 * @param next	the configuration the next script runs with
	 * @return what differs (including the profile and extra arguments), e.g. {@code headless false -> true}; or null if the same browser will do (or none started)
	 */
	@Nullable String browserChange(@Nonnull SailSettings resolved, @Nonnull BrowserConfiguration next) {
		SailSettings started = settings;
		if (started == null) {
			return null;
		}
		List<String> changes = new ArrayList<>(7);
		change(changes, "browser", started.browser(), resolved.browser());
		change(changes, "headless", Boolean.valueOf(started.headless()), Boolean.valueOf(resolved.headless()));
		change(changes, "baseUrl", started.baseUrl(), resolved.baseUrl());
		change(changes, "user agent", startedUserAgent, next.getUserAgentString());
		change(changes, "resolution", startedResolution, next.getResolution());
		change(changes, "profile", startedProfile, next.getProfile());
		change(changes, "arguments", startedArguments, next.getArguments());
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
		this.startedProfile = other.startedProfile;
		this.startedArguments = other.startedArguments;
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
	 * Refuses a profile or extra arguments for a browser that does not support them.
	 *
	 * @param browser	the browser to start
	 * @param config	the configuration to start it with
	 * @throws IllegalArgumentException	if the configuration has a profile or extra arguments and the browser is not
	 * 									Chrome or Edge
	 */
	static void requireProfileSupport(@Nonnull BrowserConfiguration.Browsers browser, @Nonnull BrowserConfiguration config) {
		boolean chromium = (browser == BrowserConfiguration.Browsers.chrome) || (browser == BrowserConfiguration.Browsers.edge);
		if ((! chromium) && ((config.getProfile() != null) || (! config.getArguments().isEmpty()))) {
			throw new IllegalArgumentException("A browser profile and extra browser arguments are supported on Chrome and Edge, not " + browser);
		}
	}

	/**
	 * Returns the command-line arguments to start Chrome with: the built-in ones, then the profile's
	 * {@code --user-data-dir} (if any), then the configuration's extra arguments.
	 *
	 * @param config	the user agent, window size, profile and extra arguments
	 * @param headless	true for a headless browser
	 * @return the arguments
	 */
	static @Nonnull List<String> chromeArguments(@Nonnull BrowserConfiguration config, boolean headless) {
		List<String> arguments = new ArrayList<>(8);
		// Chrome throttles windows it thinks are hidden (a locked screen, behind other apps): keep timers and rendering running
		arguments.add("--disable-backgrounding-occluded-windows");
		arguments.add("--disable-renderer-backgrounding");
		arguments.add("--disable-background-timer-throttling");
		String userAgentString = config.getUserAgentString();
		if (userAgentString != null) {
			arguments.add("--user-agent=" + userAgentString);
		}
		if (headless) {
			arguments.add("--headless");
			// Headless Chrome opens at 800x600, which squeezes the layouts; size it like a desktop window.
			if (config.getResolution() == null) {
				arguments.add("--window-size=1920,1080");
			}
		}
		addProfileAndArguments(arguments, config);
		return arguments;
	}

	/**
	 * Returns the command-line arguments to start Edge with: the built-in ones, then the profile's
	 * {@code --user-data-dir} (if any), then the configuration's extra arguments.
	 *
	 * @param config	the user agent, profile and extra arguments
	 * @param headless	true for a headless browser
	 * @return the arguments
	 */
	static @Nonnull List<String> edgeArguments(@Nonnull BrowserConfiguration config, boolean headless) {
		List<String> arguments = new ArrayList<>(4);
		String userAgentString = config.getUserAgentString();
		if (userAgentString != null) {
			arguments.add("--user-agent=" + userAgentString);
		}
		if (headless) {
			arguments.add("headless");
		}
		addProfileAndArguments(arguments, config);
		return arguments;
	}

	/**
	 * Builds the options to start Chrome with: {@link #chromeArguments} and the browser preferences of
	 * {@link #passwordPromptPreferences()}. A device's user agent and window size are arguments, so a mobile device
	 * and a profile get the same preferences.
	 *
	 * @param config	the user agent, window size, profile and extra arguments
	 * @param headless	true for a headless browser
	 * @return the options
	 */
	static @Nonnull ChromeOptions chromeOptions(@Nonnull BrowserConfiguration config, boolean headless) {
		ChromeOptions options = new ChromeOptions();
		options.addArguments(chromeArguments(config, headless));
		turnOffPasswordPrompts(options);
		return options;
	}

	/**
	 * Builds the options to start Edge with: {@link #edgeArguments} and the browser preferences of
	 * {@link #passwordPromptPreferences()}.
	 *
	 * @param config	the user agent, profile and extra arguments
	 * @param headless	true for a headless browser
	 * @return the options
	 */
	static @Nonnull EdgeOptions edgeOptions(@Nonnull BrowserConfiguration config, boolean headless) {
		EdgeOptions options = new EdgeOptions();
		options.addArguments(edgeArguments(config, headless));
		turnOffPasswordPrompts(options);
		return options;
	}

	/**
	 * The Chromium preferences that turn off the password manager's prompts. A SAIL user often signs in with a
	 * password that is in a leaked list (a test user whose password is its name, such as {@code admin}/{@code admin}
	 * or {@code setup}/{@code setup}). Chrome then shows its "Change your password" prompt. That prompt is tab-modal
	 * browser UI, not in the page, and while it is up every real key and mouse event to the tab is dropped, though
	 * scripts, {@code executeScript} and {@code document.hasFocus()} carry on as normal. So the run fails later, and
	 * far from the cause. A suite hands its browser from member to member, so the prompt would also carry over to the
	 * next member's sign in. The preferences also stop the "Save password?" bubble.
	 *
	 * @return the preferences, by name
	 */
	static @Nonnull Map<String, Object> passwordPromptPreferences() {
		Map<String, Object> preferences = new HashMap<>(4);
		preferences.put("profile.password_manager_leak_detection", Boolean.FALSE);
		preferences.put("credentials_enable_service", Boolean.FALSE);
		preferences.put("profile.password_manager_enabled", Boolean.FALSE);
		return preferences;
	}

	/**
	 * The {@code prefs} already set on the options (Selenium has no getter for an experimental option), which are
	 * under the options' own capability, {@code goog:chromeOptions} or {@code ms:edgeOptions}.
	 *
	 * @param options	the Chrome or Edge options
	 * @return the preferences, or null if none are set
	 */
	static @Nullable Object chromiumPreferences(@Nonnull ChromiumOptions<?> options) {
		for (Object value : options.asMap().values()) {
			if ((value instanceof Map<?, ?> map) && map.containsKey("prefs")) {
				return map.get("prefs");
			}
		}
		return null;
	}

	/**
	 * Adds {@link #passwordPromptPreferences()} to the options' {@code prefs}, keeping any other preference already
	 * set, and the value already set for one of ours, so that a caller can choose differently.
	 *
	 * @param options	the Chrome or Edge options to add them to
	 */
	@SuppressWarnings("unchecked")
	static void turnOffPasswordPrompts(@Nonnull ChromiumOptions<?> options) {
		Map<String, Object> preferences = passwordPromptPreferences();
		Object existing = chromiumPreferences(options);
		if (existing instanceof Map) {
			preferences.putAll((Map<String, Object>) existing);
		}
		options.setExperimentalOption("prefs", preferences);
	}

	private static void addProfileAndArguments(@Nonnull List<String> arguments, @Nonnull BrowserConfiguration config) {
		Path profile = config.getProfile();
		if (profile != null) {
			arguments.add("--user-data-dir=" + profile);
		}
		arguments.addAll(config.getArguments());
	}

	/**
	 * Sets the sign in page's Remember me box, as a user's click on its label would. The box itself is hidden, so it
	 * cannot be clicked; the page's own handler for the label sets it and fires its {@code change} event, which setting
	 * {@code checked} from a script would not. The box is left alone if it already shows the value.
	 *
	 * @param remember	true to tick the box, false to untick it
	 * @throws IllegalStateException	if the click does not set the box
	 */
	protected void setRememberMe(boolean remember) {
		WebElement box = driver.findElement(By.id("remember"));
		if (box.isSelected() != remember) {
			driver.findElement(By.cssSelector("label[for=\"remember\"]")).click();
		}
		if (box.isSelected() != remember) {
			throw new IllegalStateException("The Remember me box did not take the value " + remember);
		}
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
	 * Starts Chrome with the configuration's user agent, window size, profile and extra arguments.
	 *
	 * @param config	the user agent, window size and timeouts
	 * @param resolved	the settings: headless mode and the timeout
	 */
	private void setupChrome(BrowserConfiguration config, SailSettings resolved) {
		driver = new ChromeDriver(chromeOptions(config, resolved.headless()));
		
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
		driver = new EdgeDriver(edgeOptions(config, resolved.headless()));
		
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
