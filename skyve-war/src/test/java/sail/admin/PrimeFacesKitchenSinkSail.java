package sail.admin;

import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

/**
 * Runs automated SAIL tests for the kitchensink module (PrimeFaces).
 * <p>
 * Note: Replace the path in the {@code sailFile} parameter with the location of the test file you wish to run.
 */
class PrimeFacesKitchenSinkSail extends AbstractPrimeFacesInterpretedSail {

	public PrimeFacesKitchenSinkSail() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.baseUrl("http://localhost:8080/skyve/")
				.userAgentString(Devices.ipad.userAgentString));
	}

	@Test
	void test() {
		sailFile("/Users/simeonsolomou/git/skyve/skyve-war/src/test/java/sail/admin/pf-kitchensink-sail.xml");
	}
}
