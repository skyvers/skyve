package sail.kitchenSink.pf;

import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

/**
 * Runs automated SAIL tests for the kitchensink module (PrimeFaces).
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required),
 * headless unless {@code -Dsail.headless=false}.
 * The script's test strategy decides how failures are reported; the test fails with an {@link AssertionError}
 * naming each failed interaction and step.
 */
class PrimeFacesKitchenSinkSailIT extends AbstractPrimeFacesInterpretedSail {

	public PrimeFacesKitchenSinkSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	@Test
	void test() {
		sailResource("sail/kitchenSink/pf/pf-kitchensink-sail.xml");
	}

	/**
	 * {@code <switchMode />} from SmartClient back to PrimeFaces. SAIL has no step that switches away from the
	 * automation's renderer, so the switch to SmartClient is made here with PrimeFaces' own user menu (real clicks),
	 * between two automations in the same browser session.
	 */
	@Test
	void switchModeFromSmartClient() {
		sailString(automation("<login user=\"admin\" password=\"admin\" /><testSuccess /><testMenu module=\"kitchensink\" />"));
		selenium.switchToSmartClient();
		sailString(automation("<switchMode />" +
								"<testMenu module=\"kitchensink\" />" +
								"<navigateMenu module=\"kitchensink\" item=\"Actions Fixture\" />" +
								"<action name=\"RunFromToolbar\" />" +
								"<testValue binding=\"outcome\" value=\"Run From Toolbar ran\" />" +
								"<logout />"));
	}

	private static String automation(String steps) {
		return "<automation uxui=\"tablet\" userAgentType=\"tablet\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
				"<interaction name=\"Switch mode\"><method>" + steps + "</method></interaction></automation>";
	}
}
