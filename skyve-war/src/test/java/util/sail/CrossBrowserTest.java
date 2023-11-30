package util.sail;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.AfterClass;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriver.Options;
import org.openqa.selenium.WebDriver.Timeouts;

import util.JUnitMultiThreadedRunnerWithParameters;

public abstract class CrossBrowserTest {
	private static final String CHROME_DRIVER_CLASS = "org.openqa.selenium.chrome.ChromeDriver";
	private static final String CHROME_OPTIONS_CLASS = "org.openqa.selenium.chrome.ChromeOptions";
	private static final String CHROME_ADD_ARGUMENTS_METHOD = "addArguments";
	
	private static final String FIREFOX_DRIVER_CLASS = "org.openqa.selenium.firefox.FirefoxDriver";
	private static final String FIREFOX_BINARY_CLASS = "org.openqa.selenium.firefox.FirefoxBinary";
	private static final String FIREFOX_OPTIONS_CLASS = "org.openqa.selenium.firefox.FirefoxOptions";
	private static final String FIREFOX_PROFILE_CLASS = "org.openqa.selenium.firefox.FirefoxProfile";
	private static final String FIREFOX_SET_PREFERENCE_METHOD = "setPreference";
	private static final String FIREFOX_SET_PROFILE_METHOD = "setProfile";
	private static final String FIREFOX_ADD_COMMAND_LINE_OPTIONS_METHOD = "addCommandLineOptions";
	private static final String FIREFOX_SET_BINARY_METHOD = "setBinary";

	protected WebDriver driver;
	protected String baseUrl;
	protected Browsers browser;

	/**
	 * http://phantomjs.org/
	 * 
	 * @param config
	 * @throws Exception
	 */
	protected void setupPhantomJS(BrowserConfiguration config) throws Exception {
		String pathToDriver = config.getPathToDriver();
		if (pathToDriver != null) {
			System.setProperty("phantomjs.binary.path", pathToDriver);
		}

		// DesiredCapabilities capabilities = new DesiredCapabilities();
		// capabilities.setJavascriptEnabled(true);
		// capabilities.setCapability("phantomjs.page.settings.userAgent", userAgentString);
		// driver = new PhantomJSDriver(capabilities);
	
		this.browser = Browsers.phantomJS;
		setupBrowser(config);
	}
	
	/**
	 * http://chromedriver.chromium.org/downloads
	 * 
	 * @param config
	 * @throws Exception
	 */
	protected void setupChrome(BrowserConfiguration config) throws Exception {
		String pathToDriver = config.getPathToDriver();
		if (pathToDriver != null) {
			System.setProperty("webdriver.chrome.driver", pathToDriver);
		}

		// ChromeOptions options = new ChromeOptions();
		// options.addArguments("--user-agent=USER AGENT STRING");
		// driver = new ChromeDriver(options);
		ClassLoader cl = Thread.currentThread().getContextClassLoader();
		Class<?> optionsClass = cl.loadClass(CHROME_OPTIONS_CLASS);
		Object options = optionsClass.getDeclaredConstructor().newInstance();
		String userAgentString = config.getUserAgentString();
		if (userAgentString != null) {
			List<String> arguments = new ArrayList<>(2);
			arguments.add("--user-agent=" + userAgentString);
			if (config.isHeadless()) {
				arguments.add("--headless");
			}
			optionsClass.getMethod(CHROME_ADD_ARGUMENTS_METHOD, List.class).invoke(options, arguments);
		}
		Class<?> driverClass = cl.loadClass(CHROME_DRIVER_CLASS);
		driver = (WebDriver) driverClass.getConstructor(optionsClass).newInstance(options);
		
		this.browser = Browsers.chrome;
		setupBrowser(config);
	}

	/**
	 * https://github.com/mozilla/geckodriver/releases
	 * 
	 * @param config
	 * @throws Exception
	 */
	protected void setupFirefox(BrowserConfiguration config) throws Exception {
		String pathToDriver = config.getPathToDriver();
		if (pathToDriver != null) {
			System.setProperty("webdriver.gecko.driver", pathToDriver);
		}
		
		// FirefoxOptions options = new FirefoxOptions();
		// if (config.isHeadless()) {
		//	FirefoxBinary binary = new FirefoxBinary();
		//	binary.addCommandLineOptions("--headless");
		//  options.setBinary(binary);
		// }
		// if (config.getUserAgentString() != null) {
		//  FirefoxProfile profile = new FirefoxProfile();
		//  profile.setPreference("general.useragent.override", "USER AGENT STRING");
		//  options.setProfile(profile);
		// }
		// driver = new FirefoxDriver(options);
		ClassLoader cl = Thread.currentThread().getContextClassLoader();
		Class<?> optionsClass = cl.loadClass(FIREFOX_OPTIONS_CLASS);
		Object options = optionsClass.getDeclaredConstructor().newInstance();
		if (config.isHeadless()) {
			Class<?> binaryClass = cl.loadClass(FIREFOX_BINARY_CLASS);
			Object binary = binaryClass.getDeclaredConstructor().newInstance();
			binaryClass.getMethod(FIREFOX_ADD_COMMAND_LINE_OPTIONS_METHOD, String[].class).invoke(binary, new Object[] {new String[] {"--headless"}});
			optionsClass.getMethod(FIREFOX_SET_BINARY_METHOD, binaryClass).invoke(options, binary);
		}
		String userAgentString = config.getUserAgentString();
		if (userAgentString != null) {
			Class<?> profileClass = cl.loadClass(FIREFOX_PROFILE_CLASS);
			Object profile = profileClass.getDeclaredConstructor().newInstance();
			profileClass.getMethod(FIREFOX_SET_PREFERENCE_METHOD, String.class, String.class).invoke(profile, "general.useragent.override", userAgentString);
			optionsClass.getMethod(FIREFOX_SET_PROFILE_METHOD, profileClass).invoke(options, profile);
		}
		Class<?> driverClass = cl.loadClass(FIREFOX_DRIVER_CLASS);
		driver = (WebDriver) driverClass.getConstructor(optionsClass).newInstance(options);

		this.browser = Browsers.firefox;
		setupBrowser(config);
	}

	private void setupBrowser(BrowserConfiguration config) {
		this.baseUrl = config.getBaseUrl();
		Options manage = driver.manage();
		Timeouts timeouts = manage.timeouts();
		TimeUnit unit = config.getPageLoadTimeoutUnit();
		if (unit == null) {
			timeouts.pageLoadTimeout(30, TimeUnit.SECONDS);
		}
		else {
			timeouts.pageLoadTimeout(config.getPageLoadTimeout(), unit);
		}
		unit = config.getImplicitlyWaitUnit();
		if (unit == null) {
			timeouts.implicitlyWait(0, TimeUnit.MILLISECONDS);
		}
		else {
			timeouts.implicitlyWait(config.getImplicitlyWait(), unit);
		}
		unit = config.getScriptTimeoutUnit();
		if (unit != null) {
			timeouts.setScriptTimeout(config.getScriptTimeout(), unit);
		}
		Dimension resolution = config.getResolution();
		if (resolution != null) {
			manage.window().setSize(resolution);
		}
	}
	
	protected void tearDownBrowser() {
		if (driver != null) {
			driver.quit();
		}
//		Assert.assertEquals("", verificationErrors.toString());
	}
	
	@AfterClass
	public static void _waitForThreadsToComplete() throws Exception {
		while (JUnitMultiThreadedRunnerWithParameters.THREAD_COUNT.get() > 0) {
			Thread.sleep(100);
		}
	}
}
