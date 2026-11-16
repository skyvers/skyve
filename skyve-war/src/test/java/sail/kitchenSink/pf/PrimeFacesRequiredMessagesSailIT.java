package sail.kitchenSink.pf;

import org.junit.jupiter.api.Test;

import util.AbstractInterpretedSailWorkflow;

/**
 * When PrimeFaces shows required-field messages, in each PrimeFaces
 * UX/UI: never when a view first renders, nor after a rerender without client validation, nor for an action whose
 * {@code clientValidation} is false; only when an action with client validation is run, which they then stop.
 * <p>
 * Each UX/UI runs in its own browser: an iPad's user agent for {@code tablet}, an iPhone's for {@code phone}, and a
 * desktop browser for {@code external}.
 */
class PrimeFacesRequiredMessagesSailIT extends AbstractInterpretedSailWorkflow {
	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void tablet() {
		sailWorkflowString(automation("tablet", "tablet"));
	}

	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void phone() {
		sailWorkflowString(automation("phone", "phone"));
	}

	@Test
	@SuppressWarnings("java:S2699") // the SAIL script's test steps are the assertions: the harness fails the test when one fails
	void external() {
		sailWorkflowString(automation("external", "desktop"));
	}

	/**
	 * The steps, for a UX/UI. EscapingFixture's two required text fields start empty; its Trusted Display Name action
	 * (which does nothing) has {@code clientValidation="false"}, and OK has client validation. KitchenSink's required
	 * Text starts empty, and leaving its Date rerenders the view with {@code clientValidation="false"}.
	 */
	private static String automation(String uxui, String userAgentType) {
		return "<automation uxui=\"" + uxui + "\" userAgentType=\"" + userAgentType + "\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
				"<interaction name=\"Required messages (" + uxui + ")\"><method>" +
				"<login user=\"admin\" password=\"admin\" />" +
				"<testSuccess />" +
				"<comment>A view shows no required message when it first renders</comment>" +
				"<navigateEdit module=\"kitchensink\" document=\"EscapingFixture\" />" +
				"<testSuccess />" +
				"<comment>An action with clientValidation=\"false\" runs without them</comment>" +
				"<action name=\"TrustedDisplayName\" />" +
				"<testSuccess />" +
				"<comment>An action with client validation shows them, and does not run</comment>" +
				"<ok testSuccess=\"false\" />" +
				"<testFailure message=\"required message\" />" +
				"<cancel />" +
				"<comment>A rerender without client validation shows none</comment>" +
				"<navigateEdit module=\"kitchensink\" document=\"KitchenSink\" />" +
				"<testSuccess />" +
				"<dataEnter binding=\"date\" value=\"07-Nov-2025\" />" +
				"<testSuccess />" +
				"<cancel />" +
				"</method></interaction></automation>";
	}
}
