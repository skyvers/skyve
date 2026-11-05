package sail.kitchenSink.sc;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Runs automated SAIL tests for the kitchensink module (SmartClient).
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required),
 * headless unless {@code -Dsail.headless=false}.
 */
class SmartClientKitchenSinkSailIT extends AbstractSmartClientInterpretedSail {

	public SmartClientKitchenSinkSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
	}

	@Test
	void test() {
		Assertions.assertDoesNotThrow(() -> sailResource("sail/kitchenSink/sc/sc-kitchensink-sail.xml"));
	}

	/**
	 * Finds by content: list grid filters and sorting, captured generated values, options by code or label,
	 * and lookup description rows by their text.
	 */
	@Test
	void find() {
		Assertions.assertDoesNotThrow(() -> sailResource("sail/kitchenSink/sc/sc-kitchensink-sail-find.xml"));
	}
}
