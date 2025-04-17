package util.sail;

import java.time.Duration;

import org.openqa.selenium.Dimension;

/**
 * Browser configuration for SAIL (builder-of-sorts; without a build method).
 * @author mike
 */
public class BrowserConfiguration {
	public static enum Browsers {
		chrome,
		firefox,
		safari,
		edge
	}

	private Browsers browser;
	private String baseUrl;
	private String userAgentString;
	private boolean headless = false;
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
