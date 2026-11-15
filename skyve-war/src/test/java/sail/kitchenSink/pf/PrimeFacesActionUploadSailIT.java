package sail.kitchenSink.pf;

import org.junit.jupiter.api.Test;

import util.AbstractInterpretedSailWorkflow;

/**
 * An action upload that succeeds closes its upload page, which refreshes the view (runtime issue P3,
 * docs/sail-runtime-issues.md), in each PrimeFaces UX/UI: an overlay for {@code tablet} and {@code external}, a
 * full-screen dialog for {@code phone}.
 * <p>
 * The upload step closes an upload page that stays open with its close icon, as a user would, for a Skyve without the
 * fix. So the script first hides the close icons: an upload page that does not close itself then fails the step.
 */
class PrimeFacesActionUploadSailIT extends AbstractInterpretedSailWorkflow {
	@Test
	void tablet() {
		sailWorkflowString(automation("tablet", "tablet"));
	}

	@Test
	void phone() {
		sailWorkflowString(automation("phone", "phone"));
	}

	@Test
	void external() {
		sailWorkflowString(automation("external", "desktop"));
	}

	private static String automation(String uxui, String userAgentType) {
		return "<automation uxui=\"" + uxui + "\" userAgentType=\"" + userAgentType + "\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
				"<interaction name=\"Action upload (" + uxui + ")\"><method>" +
				"<login user=\"admin\" password=\"admin\" />" +
				"<testSuccess />" +
				"<navigateEdit module=\"kitchensink\" document=\"ActionsFixture\" />" +
				"<comment>No close icons: the upload page must close by itself</comment>" +
				"<execute>var s = document.createElement('style');" +
				"s.textContent = '.ui-overlaypanel-close, .ui-dialog-titlebar-close {display: none !important;}';" +
				"document.head.appendChild(s);</execute>" +
				"<upload action=\"ActionsUpload\" file=\"sail/files/sail-upload.txt\" />" +
				"<testValue binding=\"outcome\" value=\"Actions Upload ran\" />" +
				"<cancel />" +
				"</method></interaction></automation>";
	}
}
