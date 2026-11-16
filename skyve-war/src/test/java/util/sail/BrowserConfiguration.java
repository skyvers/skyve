package util.sail;

import java.time.Duration;

import org.openqa.selenium.Dimension;

import jakarta.annotation.Nonnull;

/**
 * Browser configuration for SAIL (builder-of-sorts; without a build method).
 * <p>
 * The browser, base URL and headless mode set here are code-level values of {@link SailSettings}: a {@code -D} system
 * property or environment variable beats them (e.g. {@code -Dsail.baseUrl=http://localhost:8180/skyve/
 * -Dsail.headless=false}), and so do the settings of the test that uses this configuration (see
 * {@link #settings(SailSettings.Code)}), so that a quick change in a test's {@code settings()} applies to every
 * device it uses. The getters return the resolved values.
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
	private String userAgentString;
	private Dimension resolution;
	private Duration pageLoadTimeout;
	private Duration implicitlyWait;
	private Duration scriptTimeout;
	
	public BrowserConfiguration browser(Browsers browser) {
		values.set(SailSettings.BROWSER, (browser == null) ? null : browser.name());
		return this;
	}

	public BrowserConfiguration baseUrl(String baseUrl) {
		values.set(SailSettings.BASE_URL, baseUrl);
		return this;
	}
	
	public BrowserConfiguration userAgentString(@SuppressWarnings("hiding") String userAgentString) {
		this.userAgentString = userAgentString;
		return this;
	}
	
	public BrowserConfiguration headless(boolean headless) {
		values.headless(headless);
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
	 * Resolves the settings this configuration runs with: {@code -D}, the environment, the test's settings, the values
	 * set here, and the properties file, in that order.
	 *
	 * @return the settings
	 * @throws IllegalArgumentException	if a setting is invalid
	 */
	public @Nonnull SailSettings resolve() {
		SailSettings.Layer own = values.layer("code: BrowserConfiguration");
		return (settings == null) ? SailSettings.resolve(own) : SailSettings.resolve(settings.layer(), own);
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
	 * The browser to start.
	 * @return	the resolved browser: Chrome unless set
	 */
	public @Nonnull Browsers getBrowser() {
		return resolve().browser();
	}
	
	/**
	 * The Skyve base URL (ending in "/") to run against.
	 * @return	the resolved base URL: from {@value SailSettings#BASE_URL}, its environment variable, the test's
	 * 			settings, {@link #baseUrl(String)} or the properties file.
	 * @throws IllegalStateException	if none sets it, so a run never silently targets an unintended server.
	 * @throws IllegalArgumentException	if a setting is invalid
	 */
	public @Nonnull String getBaseUrl() {
		String result = resolve().baseUrl();
		if (result == null) {
			throw new IllegalStateException("No SAIL base URL: set -D" + SailSettings.BASE_URL +
												"=http://<host>:<port>/<context>/ (or " + SailSettings.environmentName(SailSettings.BASE_URL) +
												", or baseUrl in code or the SAIL properties file) to the disposable server to test against");
		}
		return result;
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
