package sail.kitchenSink.workflow;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import util.AbstractInterpretedSailWorkflow;

/**
 * Runs a SAIL workflow against the kitchen sink whose legs are separate files: automations that cross renderers as one
 * run, sharing {@code {run}} and the captured variables (see {@link util.sail.SailWorkflow}). A workflow in one file,
 * {@code kitchensink-workflow.xml}, is a member of the kitchen sink suite ({@code KitchenSinkSuiteSailIT}).
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), headless unless {@code -Dsail.headless=false}.
 */
@Tag("sail")
@Tag("smoke")
class KitchenSinkWorkflowSailIT extends AbstractInterpretedSailWorkflow {
	/**
	 * Three script files: PrimeFaces (tablet) makes a lookup value and keeps its number, SmartClient finds it by the
	 * number and changes it, and PrimeFaces finds the change and deletes it. Each leg needs another device, so each starts
	 * a new browser.
	 */
	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void legsInSeparateFiles() {
		sailWorkflow("sail/kitchenSink/workflow/workflow-1-pf-create.xml",
						"sail/kitchenSink/workflow/workflow-2-sc-change.xml",
						"sail/kitchenSink/workflow/workflow-3-pf-delete.xml");
	}
}
