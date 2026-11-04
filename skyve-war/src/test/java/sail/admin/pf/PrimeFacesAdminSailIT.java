package sail.admin.pf;

import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

/**
 * Runs automated SAIL tests for the admin module (PrimeFaces).
 * <p>
 * Runs against {@code -Dsail.baseUrl} (default {@value BrowserConfiguration#DEFAULT_BASE_URL}),
 * headless unless {@code -Dsail.headless=false}.
 * The script's test strategy decides how failures are reported; the test fails with an {@link AssertionError}
 * naming each failed interaction and step.
 */
class PrimeFacesAdminSailIT extends AbstractPrimeFacesInterpretedSail {

	public PrimeFacesAdminSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	@Test
	void test() {
		sailResource("sail/admin/pf/pf-admin-sail.xml");
	}
}
