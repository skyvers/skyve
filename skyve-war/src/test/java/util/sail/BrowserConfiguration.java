package util.sail;

import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.openqa.selenium.Dimension;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Browser configuration for SAIL (builder-of-sorts; without a build method).
 * <p>
 * The browser, base URL and headless mode set here are this configuration's own values in {@link SailSettings}
 * (level {@link SailSettings.Level#CONFIGURATION}): a {@code -D} system property, an environment variable, the settings
 * of the test that uses this configuration (see {@link #settings(SailSettings.Code)}) and the properties file beat them,
 * e.g. {@code -Dsail.baseUrl=http://localhost:8180/skyve/ -Dsail.headless=false}. So an IT's {@code .browser(chrome)}
 * never stops the file or a test's {@code settings()} choosing another browser. The getters return the resolved values.
 * <p>
 * A {@link #fixed()} configuration resolves only its own values, for a browser that is not a SAIL run's (such as a
 * deployed-environment test's), so that no SAIL setting can send it, and its credentials, to another server.
 * <p>
 * There is deliberately no default base URL. SAIL scripts save, delete and run destructive admin
 * actions, so a run must name the (disposable) server it targets rather than fall back to a
 * developer's own server on port 8080.
 * @author mike
 */
public class BrowserConfiguration {
	/**
	 * System property naming the Skyve base URL (ending in "/") that SAIL tests run against.
	 * @deprecated	use {@link SailSettings#BASE_URL}
	 */
	@Deprecated(since = "10")
	public static final String BASE_URL_PROPERTY = SailSettings.BASE_URL;
	/**
	 * System property that, when "false", shows the browser window; SAIL browsers are headless otherwise.
	 * @deprecated	use {@link SailSettings#HEADLESS}
	 */
	@Deprecated(since = "10")
	public static final String HEADLESS_PROPERTY = SailSettings.HEADLESS;

	@SuppressWarnings("java:S115") // Enum names are browser option identifiers.
	public enum Browsers {
		chrome,
		firefox,
		safari,
		edge
	}

	/** The code-level values set here: browser, base URL and headless mode. */
	private final SailSettings.Code values = SailSettings.code();
	/** The settings of the test that uses this configuration, which beat its own values; or null. */
	private SailSettings.Code settings;
	/** True to resolve only the values set here; see {@link #fixed()}. */
	private boolean fixed;
	private String userAgentString;
	private Dimension resolution;
	private Duration pageLoadTimeout;
	private Duration implicitlyWait;
	private Duration scriptTimeout;
	private Path profile;
	private final List<String> arguments = new ArrayList<>();
	
	/**
	 * Sets this configuration's own browser, which a run's {@value SailSettings#BROWSER} (from {@code -D}, the
	 * environment, the test's settings or the properties file) beats.
	 *
	 * @param browser	the browser, or null to clear it (Chrome, unless a setting names one)
	 * @return this configuration
	 */
	public BrowserConfiguration browser(@Nullable Browsers browser) {
		values.set(SailSettings.BROWSER, (browser == null) ? null : browser.name());
		return this;
	}

	/**
	 * Sets this configuration's own base URL, which a run's {@value SailSettings#BASE_URL} (from {@code -D}, the
	 * environment, the test's settings or the properties file) beats.
	 *
	 * @param baseUrl	an http or https URL ending in {@code /}, or null to clear it
	 * @return this configuration
	 */
	public BrowserConfiguration baseUrl(@Nullable String baseUrl) {
		values.set(SailSettings.BASE_URL, baseUrl);
		return this;
	}
	
	public BrowserConfiguration userAgentString(@SuppressWarnings("hiding") String userAgentString) {
		this.userAgentString = userAgentString;
		return this;
	}
	
	/**
	 * Sets this configuration's own headless mode, which a run's {@value SailSettings#HEADLESS} (from {@code -D}, the
	 * environment, the test's settings or the properties file) beats.
	 *
	 * @param headless	false to show the browser window
	 * @return this configuration
	 */
	public BrowserConfiguration headless(boolean headless) {
		values.headless(headless);
		return this;
	}

	/**
	 * Makes this configuration resolve only the values set on it: no {@code -D} system property, environment variable,
	 * test settings or properties file applies. For a browser that is not a SAIL run's, such as a deployed-environment
	 * test's, so that a SAIL setting cannot send it, and its credentials, to another server.
	 *
	 * @return this configuration
	 */
	public @Nonnull BrowserConfiguration fixed() {
		fixed = true;
		return this;
	}

	/**
	 * Uses this configuration under a test's code-level settings, which beat the values set here.
	 * The test bases and {@link SailWorkflow} call it with their {@code settings()}.
	 *
	 * <p>Side effects: replaces the settings given before.
	 *
	 * @param code	the test's settings; shared, so later changes to them apply
	 * @return this configuration
	 */
	public @Nonnull BrowserConfiguration settings(@Nonnull SailSettings.Code code) {
		this.settings = code;
		return this;
	}

	/**
	 * Resolves the settings this configuration runs with: {@code -D}, the environment, the test's settings, the
	 * properties file and the values set here, in that order; or, for a {@link #fixed()} configuration, only the values
	 * set here.
	 *
	 * <p>Side effects: reads the properties file, if there is one.
	 *
	 * @return the settings
	 * @throws IllegalArgumentException	if a setting is invalid
	 */
	public @Nonnull SailSettings resolve() {
		return resolve(SailSettings.Environment.current());
	}

	/** Resolves the settings from the given sources (none, for a {@link #fixed()} configuration); see {@link #resolve()}. */
	@Nonnull SailSettings resolve(@Nonnull SailSettings.Environment environment) {
		SailSettings.Layer own = values.layer(SailSettings.Level.CONFIGURATION, "BrowserConfiguration");
		List<SailSettings.Layer> layers = ((settings == null) || fixed) ? List.of(own) : List.of(settings.layer(), own);
		return SailSettings.resolve(fixed ? SailSettings.Environment.none() : environment, layers);
	}
	
	/**
	 * Sets the browser's profile directory, so that the browser is a device that keeps its IndexedDB, CacheStorage,
	 * localStorage, service worker registration and persistent cookies (not session cookies) when it is stopped and started again
	 * on a configuration with the same profile. Two profiles are two devices. Chrome and Edge only.
	 *
	 * @param userDataDir	the directory, made absolute here (the caller owns and deletes it); or null for a fresh
	 * 						temporary profile, the default
	 * @return this configuration
	 */
	public @Nonnull BrowserConfiguration profile(@Nullable Path userDataDir) {
		this.profile = (userDataDir == null) ? null : userDataDir.toAbsolutePath().normalize();
		return this;
	}

	/**
	 * Appends extra browser arguments, after the built-in ones, e.g. {@code --use-fake-device-for-media-stream}.
	 * Chrome and Edge only.
	 *
	 * @param extra	the arguments, as the browser takes them on its command line; none may be null or
	 * 				{@code --user-data-dir}, which {@link #profile(Path)} sets
	 * @return this configuration
	 * @throws NullPointerException	if an argument is null
	 * @throws IllegalArgumentException	if an argument sets the user data directory
	 */
	public @Nonnull BrowserConfiguration arguments(@Nonnull String... extra) {
		for (String argument : extra) {
			if (argument.startsWith("--user-data-dir")) {
				throw new IllegalArgumentException("Set the browser's user data directory with profile(), not the argument " + argument);
			}
			arguments.add(argument);
		}
		return this;
	}

	public BrowserConfiguration resolution(int width, int height) {
		this.resolution = new Dimension(width, height);
		return this;
	}
	
	public BrowserConfiguration pageLoadTimeout(Duration duration) {
		this.pageLoadTimeout = duration;
		return this;
	}
	
	public BrowserConfiguration implicitlyWait(Duration duration) {
		this.implicitlyWait = duration;
		return this;
	}
	
	public BrowserConfiguration scriptTimeout(Duration duration) {
		this.scriptTimeout = duration;
		return this;
	}

	/**
	 * Returns the resolved browser.
	 * @return	Chrome unless a setting or {@link #browser(Browsers)} names another
	 * @throws IllegalArgumentException	if a setting is invalid
	 */
	public @Nonnull Browsers getBrowser() {
		return resolve().browser();
	}

	/**
	 * Returns the resolved Skyve base URL (ending in "/") to run against.
	 * @return	the base URL from {@value SailSettings#BASE_URL}, its environment variable, the test's settings, the
	 * 			properties file or {@link #baseUrl(String)}.
	 * @throws IllegalStateException	if none sets it, so a run never silently targets an unintended server.
	 * @throws IllegalArgumentException	if a setting is invalid
	 */
	public @Nonnull String getBaseUrl() {
		return resolve().requireBaseUrl();
	}

	public String getUserAgentString() {
		return userAgentString;
	}

	/**
	 * Indicates if the browser is headless.
	 * @return	the resolved headless mode: true unless set to false
	 */
	public boolean isHeadless() {
		return resolve().headless();
	}
	
	/**
	 * @return	the absolute directory, or null for a fresh temporary profile
	 */
	public @Nullable Path getProfile() {
		return profile;
	}

	/**
	 * @return	an unmodifiable view of the arguments given to {@link #arguments(String...)}, in order
	 */
	public @Nonnull List<String> getArguments() {
		return Collections.unmodifiableList(arguments);
	}

	public Dimension getResolution() {
		return resolution;
	}

	public Duration getPageLoadTimeout() {
		return pageLoadTimeout;
	}

	public Duration getImplicitlyWait() {
		return implicitlyWait;
	}

	public Duration getScriptTimeout() {
		return scriptTimeout;
	}
}
