package util.sail;

import java.util.concurrent.TimeUnit;

import org.openqa.selenium.Dimension;

/**
 * Browser configuration for SAIL (builder-of-sorts; without a build method).
 * @author mike
 */
public class BrowserConfiguration {
	private String baseUrl;
	private String pathToDriver;
	private String userAgentString;
	private boolean headless = false;
	private Dimension resolution;
	private long pageLoadTimeout;
	private TimeUnit pageLoadTimeoutUnit;
	private long implicitlyWait;
	private TimeUnit implicitlyWaitUnit;
	private long scriptTimeout;
	private TimeUnit scriptTimeoutUnit;
	
	public BrowserConfiguration baseUrl(@SuppressWarnings("hiding") String baseUrl) {
		this.baseUrl = baseUrl;
		return this;
	}
	
	public BrowserConfiguration pathToDriver(@SuppressWarnings("hiding") String pathToDriver) {
		this.pathToDriver = pathToDriver;
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
	
	public BrowserConfiguration pageLoadTimeout(long time, TimeUnit unit) {
		this.pageLoadTimeout = time;
		this.pageLoadTimeoutUnit = unit;
		return this;
	}
	
	public BrowserConfiguration implicitlyWait(long time, TimeUnit unit) {
		this.implicitlyWait = time;
		this.implicitlyWaitUnit = unit;
		return this;
	}
	
	public BrowserConfiguration scriptTimeout(long time, TimeUnit unit) {
		this.scriptTimeout = time;
		this.scriptTimeoutUnit = unit;
		return this;
	}

	String getBaseUrl() {
		return baseUrl;
	}

	String getPathToDriver() {
		return pathToDriver;
	}

	String getUserAgentString() {
		return userAgentString;
	}

	boolean isHeadless() {
		return headless;
	}
	
	Dimension getResolution() {
		return resolution;
	}

	long getPageLoadTimeout() {
		return pageLoadTimeout;
	}

	TimeUnit getPageLoadTimeoutUnit() {
		return pageLoadTimeoutUnit;
	}

	long getImplicitlyWait() {
		return implicitlyWait;
	}

	TimeUnit getImplicitlyWaitUnit() {
		return implicitlyWaitUnit;
	}

	long getScriptTimeout() {
		return scriptTimeout;
	}

	TimeUnit getScriptTimeoutUnit() {
		return scriptTimeoutUnit;
	}
}
