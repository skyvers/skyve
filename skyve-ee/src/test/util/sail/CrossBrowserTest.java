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
	private static final String FIREFOX_OPTIONS_CLASS = "org.openqa.selenium.firefox.FirefoxOptions";
	private static final String FIREFOX_PROFILE_CLASS = "org.openqa.selenium.firefox.FirefoxProfile";
	private static final String FIREFOX_SET_PREFERENCE_METHOD = "setPreference";
	private static final String FIREFOX_SET_PROFILE_METHOD = "setProfile";

	protected WebDriver driver;
	protected String baseUrl;
	protected Browsers browser;

	/**
	 * http://phantomjs.org/
	 * 
	 * @param baseUrl
	 * @param pathToDriver
	 * @throws Exception
	 */
	protected void setupPhantomJS(String baseUrl, String pathToDriver) throws Exception {
		setupPhantomJS(baseUrl, pathToDriver, null, null);
	}

	protected void setupPhantomJS(String baseUrl, String pathToDriver, String userAgentString) throws Exception {
		setupPhantomJS(baseUrl, pathToDriver, userAgentString, null);
	}
	
	protected void setupPhantomJS(String baseUrl, String pathToDriver, String userAgentString, Dimension resolution) throws Exception {
		System.setProperty("phantomjs.binary.path", pathToDriver);

		// DesiredCapabilities capabilities = new DesiredCapabilities();
		// capabilities.setJavascriptEnabled(true);
		// capabilities.setCapability("phantomjs.page.settings.userAgent", userAgentString);
		// driver = new PhantomJSDriver(capabilities);
	
		this.browser = Browsers.phantomJS;
		setupBrowser(baseUrl, resolution);
	}
	
	protected void setupChrome(String baseUrl, String pathToDriver) throws Exception {
		setupChrome(baseUrl, pathToDriver, null, null);
	}

	protected void setupChrome(String baseUrl, String pathToDriver, String userAgentString) throws Exception {
		setupChrome(baseUrl, pathToDriver, userAgentString, null);
	}
	
	protected void setupChrome(String baseUrl, String pathToDriver, String userAgentString, Dimension resolution) throws Exception {
		System.setProperty("webdriver.chrome.driver", pathToDriver);

		// ChromeOptions options = new ChromeOptions();
		// options.addArguments("--user-agent=USER AGENT STRING");
		// driver = new ChromeDriver(options);
		ClassLoader cl = Thread.currentThread().getContextClassLoader();
		Class<?> optionsClass = cl.loadClass(CHROME_OPTIONS_CLASS);
		Object options = optionsClass.newInstance();
		if (userAgentString != null) {
			List<String> arguments = new ArrayList<>();
			arguments.add("--user-agent=" + userAgentString);
			optionsClass.getMethod(CHROME_ADD_ARGUMENTS_METHOD, List.class).invoke(options, arguments);
		}
		Class<?> driverClass = cl.loadClass(CHROME_DRIVER_CLASS);
		driver = (WebDriver) driverClass.getConstructor(optionsClass).newInstance(options);

		this.browser = Browsers.chrome;
		setupBrowser(baseUrl, resolution);
	}

	/**
	 * https://github.com/mozilla/geckodriver/releases
	 * @param baseUrl
	 * @param pathToDriver
	 * @throws Exception
	 */
	protected void setupFirefox(String baseUrl, String pathToDriver) throws Exception {
		setupFirefox(baseUrl, pathToDriver, null, null);
	}

	protected void setupFirefox(String baseUrl, String pathToDriver, String userAgentString) throws Exception {
		setupFirefox(baseUrl, pathToDriver, userAgentString, null);
	}
	
	protected void setupFirefox(String baseUrl, String pathToDriver, String userAgentString, Dimension resolution) throws Exception {
		System.setProperty("webdriver.gecko.driver", pathToDriver);
		
		// FirefoxOptions options = new FirefoxOptions();
		// FirefoxProfile profile = new FirefoxProfile();
		// profile.setPreference("general.useragent.override", "USER AGENT STRING");
		// options.setProfile(profile);
		// driver = new FirefoxDriver(options);
		ClassLoader cl = Thread.currentThread().getContextClassLoader();
		Class<?> optionsClass = cl.loadClass(FIREFOX_OPTIONS_CLASS);
		Object options = optionsClass.newInstance();
		Class<?> profileClass = cl.loadClass(FIREFOX_PROFILE_CLASS);
		Object profile = profileClass.newInstance();
		if (userAgentString != null) {
			profileClass.getMethod(FIREFOX_SET_PREFERENCE_METHOD, String.class, String.class).invoke(profile, "general.useragent.override", userAgentString);
		}
		optionsClass.getMethod(FIREFOX_SET_PROFILE_METHOD, profileClass).invoke(options, profile);
		Class<?> driverClass = cl.loadClass(FIREFOX_DRIVER_CLASS);
		driver = (WebDriver) driverClass.getConstructor(optionsClass).newInstance(options);

		this.browser = Browsers.firefox;
		setupBrowser(baseUrl, resolution);
	}

	private void setupBrowser(String baseUrl, Dimension resolution) {
		this.baseUrl = baseUrl;
		Options manage = driver.manage();
		Timeouts timeouts = manage.timeouts();
		timeouts.pageLoadTimeout(30, TimeUnit.SECONDS);
		timeouts.implicitlyWait(0, TimeUnit.MILLISECONDS);
		if (resolution != null) {
			manage.window().setSize(resolution);
		}
	}
	
	protected void tearDownBrowser() {
		driver.quit();
//		Assert.assertEquals("", verificationErrors.toString());
	}
	
	@AfterClass
	public static void _waitForThreadsToComplete() throws Exception {
		while (JUnitMultiThreadedRunnerWithParameters.THREAD_COUNT.get() > 0) {
			Thread.sleep(100);
		}
	}
}
