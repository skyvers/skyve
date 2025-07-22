package util.sail;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

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

import util.sail.BrowserConfiguration.Browsers;

abstract class CrossBrowserSelenium {
	protected WebDriver driver;
	protected BrowserConfiguration configuration;
	protected Actions actions;
	
	public void startBrowser(@SuppressWarnings("hiding") BrowserConfiguration configuration) {
		this.configuration = configuration;
		Browsers browser = configuration.getBrowser();
		if (browser == null) {
			setupChrome(configuration);
		}
		else {
			switch (browser) {
				case chrome:
					setupChrome(configuration);
					break;
				case firefox:
					setupFirefox(configuration);
					break;
				case safari:
					setupSafari(configuration);
					break;
				case edge:
					setupEdge(configuration);
					break;
				default:
					throw new IllegalStateException("Browser not supported");
			}
		}
		actions = new Actions(driver);
	}
	
	public void stopBrowser() {
		if (driver != null) {
			driver.quit();
		}
	}

	/**
	 * 
	 * @param config
	 * @throws Exception
	 */
	private void setupChrome(BrowserConfiguration config) {
		List<String> arguments = new ArrayList<>(2);
		String userAgentString = config.getUserAgentString();
		if (userAgentString != null) {
			arguments.add("--user-agent=" + userAgentString);
		}
		if (config.isHeadless()) {
			arguments.add("--headless");
		}
		
		ChromeOptions options = new ChromeOptions();
		options.addArguments(arguments);
		driver = new ChromeDriver(options);
		
		setupBrowser(config);
	}

	/**
	 * 
	 * @param config
	 * @throws Exception
	 */
	private void setupFirefox(BrowserConfiguration config) {
		FirefoxOptions options = new FirefoxOptions();
		if (config.isHeadless()) {
			options.addArguments("-headless");
		}
		FirefoxProfile profile = new FirefoxProfile();
		String userAgentString = config.getUserAgentString();
		if (userAgentString != null) {
			profile.setPreference("general.useragent.override", "USER AGENT STRING");
		}
		options.setProfile(profile);
		driver = new FirefoxDriver(options);

		setupBrowser(config);
	}
	
	/**
	 * run "safaridriver --enable" in a terminal.
	 * Safari does not have a headless option.
	 * 
	 * @param config
	 */
	private void setupSafari(BrowserConfiguration config) {
		SafariOptions options = new SafariOptions();
		driver = new SafariDriver(options);
		
		setupBrowser(config);
	}

	private void setupEdge(BrowserConfiguration config) {
		List<String> arguments = new ArrayList<>(2);
		String userAgentString = config.getUserAgentString();
		if (userAgentString != null) {
			arguments.add("--user-agent=" + userAgentString);
		}
		if (config.isHeadless()) {
			arguments.add("headless");
		}
		
		EdgeOptions options = new EdgeOptions();
		options.addArguments(arguments);
		driver = new EdgeDriver(options);
		
		setupBrowser(config);
	}
	
	private void setupBrowser(BrowserConfiguration config) {
		Options manage = driver.manage();
		
		Timeouts timeouts = manage.timeouts();
		Duration duration = config.getPageLoadTimeout();
		timeouts.pageLoadTimeout((duration == null) ? Duration.ofSeconds(30) : duration);
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
