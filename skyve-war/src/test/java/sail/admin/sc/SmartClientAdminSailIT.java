package sail.admin.sc;

import org.junit.jupiter.api.Test;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Runs automated SAIL tests for the admin module (SmartClient).
 * <p>
 * Note: Replace the path in the {@code sailFile} parameter with the location of the test file you wish to run.
 */
class SmartClientAdminSailIT extends AbstractSmartClientInterpretedSail {

	public SmartClientAdminSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.baseUrl("http://localhost:8080/skyve/"));
	}

	@Test
	void test() {
		sailFile("/Users/simeonsolomou/git/skyve/skyve-war/src/test/java/sail/admin/sc/sc-admin-sail.xml");
	}
}
