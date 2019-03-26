
import java.io.File;

import org.junit.Before;
import org.junit.Test;

import util.sail.BrowserConfiguration;
import util.sail.PrimeFacesTest;

/**
 * This test is bundled with the sail zip.
 * It is replaced when a user places the Sail.jar as this jar is before test.jar in the classpath.
 */
public class Sail extends PrimeFacesTest {
	@Before
	public void setup() throws Exception {
		setupChrome(new BrowserConfiguration().pathToDriver((File.pathSeparatorChar == ':') ? "./chromedriver" : "./chromedriver.exe"));
	}
	
	@Test
	public void open() {
		driver.get("https://foundry.skyve.org/");
	}
}
