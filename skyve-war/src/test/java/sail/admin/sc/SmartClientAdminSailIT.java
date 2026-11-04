package sail.admin.sc;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Runs automated SAIL tests for the admin module (SmartClient).
 * <p>
 * Runs against {@code -Dsail.baseUrl} (default {@value BrowserConfiguration#DEFAULT_BASE_URL}),
 * headless unless {@code -Dsail.headless=false}.
 */
class SmartClientAdminSailIT extends AbstractSmartClientInterpretedSail {

	public SmartClientAdminSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
	}

	@Test
	void test() {
		Assertions.assertDoesNotThrow(() -> sailResource("sail/admin/sc/sc-admin-sail.xml"));
	}
}
