package sail.kitchenSink;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import util.AbstractInterpretedSailWorkflow;

/**
 * Runs the kitchen sink conformance suite, {@code sail/kitchenSink/kitchensink.suite.xml}: the PrimeFaces and SmartClient
 * conformance scripts and a workflow across both renderers, as independent members (see the suite file).
 * <p>
 * The workflow signs in with {@code {var:password}}: this test supplies it in code, as a quick change in a test would;
 * {@code -Dsail.var.password} or {@code SAIL_VAR_PASSWORD} beats it, and is how a run without this class (SailRunIT)
 * supplies it. Runs against {@code -Dsail.baseUrl} (required). Quick and non-destructive: its members delete what they make.
 */
@Tag("sail")
@Tag("smoke")
class KitchenSinkSuiteSailIT extends AbstractInterpretedSailWorkflow {
	KitchenSinkSuiteSailIT() {
		// the bootstrap user's password on a test server; never in the scripts
		settings().variable("password", "admin");
	}

	@Test
	@SuppressWarnings("java:S2699") // the suite's test steps are the assertions: the harness fails the test when a member fails
	void suite() {
		sailSuite("sail/kitchenSink/kitchensink.suite.xml");
	}
}
