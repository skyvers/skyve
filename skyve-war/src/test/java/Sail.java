
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.PrimeFacesSelenium;

/**
 * This test is bundled with the sail zip.
 * It is replaced when a user places the Sail.jar as this jar is before test.jar in the classpath.
 */
@Disabled("This is replaced")
class Sail extends PrimeFacesSelenium {
	@BeforeEach
	void setup() {
		startBrowser(new BrowserConfiguration().browser(Browsers.chrome));
	}
	
	@AfterEach
	void tearDown() {
		stopBrowser();
	}
	
	@Test
	void open() {
		driver.get("https://foundry.skyve.org/");
	}
}
