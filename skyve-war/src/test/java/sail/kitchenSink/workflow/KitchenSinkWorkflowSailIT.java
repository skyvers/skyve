package sail.kitchenSink.workflow;

import org.junit.jupiter.api.Test;

import util.AbstractInterpretedSailWorkflow;

/**
 * Runs SAIL workflows against the kitchen sink: automations that cross renderers as one run, sharing {@code {run}} and
 * the captured variables (see {@link util.sail.SailWorkflow}).
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), headless unless {@code -Dsail.headless=false}.
 */
class KitchenSinkWorkflowSailIT extends AbstractInterpretedSailWorkflow {
	/**
	 * Three script files: PrimeFaces (tablet) makes a lookup value and keeps its number, SmartClient finds it by the
	 * number and changes it, and PrimeFaces finds the change and deletes it. Each leg needs another device, so each starts
	 * a new browser.
	 */
	@Test
	void legsInSeparateFiles() {
		sailWorkflow("sail/kitchenSink/workflow/workflow-1-pf-create.xml",
						"sail/kitchenSink/workflow/workflow-2-sc-change.xml",
						"sail/kitchenSink/workflow/workflow-3-pf-delete.xml");
	}

	/**
	 * One {@code <workflow>} file: PrimeFaces (external, desktop) makes a lookup value, then SmartClient, in the same
	 * browser, finds it by its number and deletes it.
	 */
	@Test
	void oneWorkflowFile() {
		sailWorkflow("sail/kitchenSink/workflow/kitchensink-workflow.xml");
	}
}
