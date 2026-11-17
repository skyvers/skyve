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
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void dates() {
		sailResource("sail/kitchenSink/sc/sc-converters-dates.xml");
	}

	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void dateTimes() {
		sailResource("sail/kitchenSink/sc/sc-converters-dateTimes.xml");
	}

	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void timestamps() {
		sailResource("sail/kitchenSink/sc/sc-converters-timestamps.xml");
	}

	/**
	 * Date times and timestamps whose converters show only the date ({@code *_DateTime}, {@code *_Timestamp}): a date
	 * time to SmartClient, with a date-only field, filtered by the whole local day.
	 */
	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void dateOnly() {
		sailResource("sail/kitchenSink/sc/sc-converters-dateOnly.xml");
	}

	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void times() {
		sailResource("sail/kitchenSink/sc/sc-converters-times.xml");
	}

	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void numbers() {
		sailResource("sail/kitchenSink/sc/sc-converters-numbers.xml");
	}
}
