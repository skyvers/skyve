package sail.kitchenSink.pf;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

/**
 * Runs the converters conformance scripts (PrimeFaces): for each group of converters on
 * {@code kitchensink.ConvertersFixture}, data entry in form fields and inline grid cells, checked after the save and
 * once reopened from the list, then filters on each column of the Vue list grid and the PrimeFaces data table, written
 * as the cells show the values.
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), headless unless {@code -Dsail.headless=false}.
 */
@Tag("sail")
class PrimeFacesConvertersSailIT extends AbstractPrimeFacesInterpretedSail {

	public PrimeFacesConvertersSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void dates() {
		sailResource("sail/kitchenSink/pf/pf-converters-dates.xml");
	}

	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void dateTimes() {
		sailResource("sail/kitchenSink/pf/pf-converters-dateTimes.xml");
	}

	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void timestamps() {
		sailResource("sail/kitchenSink/pf/pf-converters-timestamps.xml");
	}

	/**
	 * Date times and timestamps whose converters show only the date ({@code *_DateTime}, {@code *_Timestamp}).
	 */
	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void dateOnly() {
		sailResource("sail/kitchenSink/pf/pf-converters-dateOnly.xml");
	}

	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void times() {
		sailResource("sail/kitchenSink/pf/pf-converters-times.xml");
	}

	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void numbers() {
		sailResource("sail/kitchenSink/pf/pf-converters-numbers.xml");
	}
}
