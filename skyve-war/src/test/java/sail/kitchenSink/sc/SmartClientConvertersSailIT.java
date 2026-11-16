package sail.kitchenSink.sc;

import org.junit.jupiter.api.Test;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * Runs the converters conformance scripts (SmartClient): for each group of converters on
 * {@code kitchensink.ConvertersFixture}, data entry in form fields and inline grid cells, checked after the save and
 * once reopened from the list, then filters on each column of the list view's grid, written as the cells show the
 * values.
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), headless unless {@code -Dsail.headless=false}.
 */
class SmartClientConvertersSailIT extends AbstractSmartClientInterpretedSail {

	public SmartClientConvertersSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
	}

	@Test
	void dates() {
		sailResource("sail/kitchenSink/sc/sc-converters-dates.xml");
	}

	@Test
	void dateTimes() {
		sailResource("sail/kitchenSink/sc/sc-converters-dateTimes.xml");
	}

	@Test
	void timestamps() {
		sailResource("sail/kitchenSink/sc/sc-converters-timestamps.xml");
	}

	@Test
	void times() {
		sailResource("sail/kitchenSink/sc/sc-converters-times.xml");
	}

	@Test
	void numbers() {
		sailResource("sail/kitchenSink/sc/sc-converters-numbers.xml");
	}
}
