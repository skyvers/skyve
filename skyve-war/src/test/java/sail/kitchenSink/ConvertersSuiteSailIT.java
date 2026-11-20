package sail.kitchenSink;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import util.AbstractInterpretedSailWorkflow;

/**
 * Runs the converters conformance suite, {@code sail/kitchenSink/converters.suite.xml}: a script per group of converters
 * on {@code kitchensink.ConvertersFixture}, in PrimeFaces (tablet) and in SmartClient, as independent members (see the
 * suite file). Each enters values in form fields and inline grid cells, checks them after the save and once reopened
 * from the list, then filters each list grid column, written as the cells show the values.
 * <p>
 * Runs against {@code -Dsail.baseUrl} (required). Non-destructive: its members make only their own records.
 * {@code -Dsail.suite.only=pf/} (or {@code sc/}, or a file such as {@code pf/pf-converters-dates}) runs part of it.
 */
@Tag("sail")
class ConvertersSuiteSailIT extends AbstractInterpretedSailWorkflow {
	@Test
	@SuppressWarnings("java:S2699") // the suite's test steps are the assertions: the harness fails the test when a member fails
	void suite() {
		sailSuite("sail/kitchenSink/converters.suite.xml");
	}
}
