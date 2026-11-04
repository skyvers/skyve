package sail.kitchenSink.pf;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

/**
 * Runs automated SAIL tests for the kitchensink module (PrimeFaces).
 * <p>
 * Runs against {@code -Dsail.baseUrl} (default {@value BrowserConfiguration#DEFAULT_BASE_URL}),
 * headless unless {@code -Dsail.headless=false}.
 */
class PrimeFacesKitchenSinkSailIT extends AbstractPrimeFacesInterpretedSail {

	public PrimeFacesKitchenSinkSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	@Test
	void test() {
		Assertions.assertDoesNotThrow(() -> sailResource("sail/kitchenSink/pf/pf-kitchensink-sail.xml"));
	}
}
