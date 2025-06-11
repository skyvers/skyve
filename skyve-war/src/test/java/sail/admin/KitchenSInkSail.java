package sail.admin;

import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

class KitchenSInkSail extends AbstractPrimeFacesInterpretedSail {
	public KitchenSInkSail() {
		super(new BrowserConfiguration()
						.browser(Browsers.chrome)
						.baseUrl("http://localhost:8080/skyve/")
						.userAgentString(Devices.ipad.userAgentString));
	}
	
	@Test
	void test() {
		sailFile("/Users/mike/_/skyve/skyve-war/src/test/java/sail/admin/kitchensink-sail.xml");
	}
}
