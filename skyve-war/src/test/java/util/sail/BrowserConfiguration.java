package util.sail;

import java.time.Duration;

import org.openqa.selenium.Dimension;

/**
 * Browser configuration for SAIL (builder-of-sorts; without a build method).
 * <p>
 * The base URL and headless mode default from the {@value #BASE_URL_PROPERTY} and
 * {@value #HEADLESS_PROPERTY} system properties, so a test run can point at another server
 * or show the browser without code changes, e.g.
 * {@code -Dsail.baseUrl=http://localhost:8180/skyve/ -Dsail.headless=false}.
 * @author mike
 */
public class BrowserConfiguration {
	/** System property naming the Skyve base URL (ending in "/") that SAIL tests run against. */
	public static final String BASE_URL_PROPERTY = "sail.baseUrl";
	/** The base URL used when {@value #BASE_URL_PROPERTY} is not set. */
	public static final String DEFAULT_BASE_URL = "http://localhost:8080/skyve/";
	/** System property that, when "false", shows the browser window; SAIL browsers are headless otherwise. */
	public static final String HEADLESS_PROPERTY = "sail.headless";

	@SuppressWarnings("java:S115") // Enum names are browser option identifiers.
	public enum Browsers {
		chrome,
		firefox,
		safari,
		edge
	}

	private Browsers browser;
	private String baseUrl = System.getProperty(BASE_URL_PROPERTY, DEFAULT_BASE_URL);
	private String userAgentString;
	private boolean headless = Boolean.parseBoolean(System.getProperty(HEADLESS_PROPERTY, "true"));
	private Dimension resolution;
	private Duration pageLoadTimeout;
	private Duration implicitlyWait;
	private Duration scriptTimeout;
	
	public BrowserConfiguration browser(@SuppressWarnings("hiding") Browsers browser) {
		this.browser = browser;
		return this;
	}

	public BrowserConfiguration baseUrl(@SuppressWarnings("hiding") String baseUrl) {
		this.baseUrl = baseUrl;
		return this;
	}
	
	public BrowserConfiguration userAgentString(@SuppressWarnings("hiding") String userAgentString) {
		this.userAgentString = userAgentString;
		return this;
	}
	
	public BrowserConfiguration headless(@SuppressWarnings("hiding") boolean headless) {
		this.headless = headless;
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

	public Browsers getBrowser() {
		return browser;
	}
	
	public String getBaseUrl() {
		return baseUrl;
	}

	public String getUserAgentString() {
		return userAgentString;
	}

	public boolean isHeadless() {
		return headless;
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
