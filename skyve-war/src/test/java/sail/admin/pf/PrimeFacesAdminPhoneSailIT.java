package sail.admin.pf;

import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

/**
 * Smoke-tests the admin module in the PrimeFaces phone UX/UI: each phone menu item opens.
 * <p>
 * Runs against {@code -Dsail.baseUrl}, headless unless {@code -Dsail.headless=false}.
 */
class PrimeFacesAdminPhoneSailIT extends AbstractPrimeFacesInterpretedSail {
	public PrimeFacesAdminPhoneSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.iphone.userAgentString)
				.resolution(Devices.iphone.resolution.getWidth(), Devices.iphone.resolution.getHeight()));
	}

	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void test() {
		sailResource("sail/admin/pf/admin-phone-smoke.xml");
	}
}
