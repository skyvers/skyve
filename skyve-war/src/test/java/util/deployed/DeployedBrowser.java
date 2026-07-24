package util.deployed;

import java.io.IOException;
import java.net.HttpCookie;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriverException;

import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.PrimeFacesSelenium;

/** Reuses the SAIL PrimeFaces browser lifecycle for deployed integration suites. */
public final class DeployedBrowser extends PrimeFacesSelenium implements AutoCloseable {
	private final DeployedTestConfiguration deployedConfiguration;

	public DeployedBrowser(DeployedTestConfiguration configuration) {
		deployedConfiguration = configuration;
	}

	/** Starts a Chrome session with shared deployed-test defaults. */
	public void start() {
		BrowserConfiguration browser = new BrowserConfiguration().browser(Browsers.chrome)
				.baseUrl(deployedConfiguration.getBaseUrl().toString()).headless(deployedConfiguration.isHeadless())
				.pageLoadTimeout(deployedConfiguration.getTimeout()).scriptTimeout(deployedConfiguration.getTimeout());
		startBrowser(browser);
	}

	/** Signs in with protected environment credentials. */
	public void login() {
		String username = deployedConfiguration.getUsername();
		String password = deployedConfiguration.getPassword();
		if ((username == null) || (password == null)) {
			throw new IllegalStateException("Deployed browser login credentials were not supplied");
		}
		login(username, password);
	}

	/** Writes a PNG screenshot without embedding it in test output. */
	public Path screenshot(Path destination) throws IOException {
		byte[] png = ((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES);
		Files.createDirectories(destination.getParent());
		return Files.write(destination, png);
	}

	/** Returns the current page source for failure diagnostics. */
	public String pageSource() {
		return driver.getPageSource();
	}

	/** Returns browser capabilities when the driver exposes them. */
	public String browserDescription() {
		return driver.toString();
	}

	/** Returns the authenticated browser cookies for correlation and direct postback clients. */
	public List<HttpCookie> authenticatedCookies() {
		List<HttpCookie> result = new ArrayList<>();
		for (org.openqa.selenium.Cookie browserCookie : driver.manage().getCookies()) {
			HttpCookie cookie = new HttpCookie(browserCookie.getName(), browserCookie.getValue());
			cookie.setPath(browserCookie.getPath());
			cookie.setSecure(browserCookie.isSecure());
			cookie.setHttpOnly(browserCookie.isHttpOnly());
			result.add(cookie);
		}
		return result;
	}

	@Override
	public void close() {
		try {
			stopBrowser();
		}
		catch (@SuppressWarnings("unused") WebDriverException e) {
			// Cleanup must not hide the test failure that triggered it.
		}
	}
}
