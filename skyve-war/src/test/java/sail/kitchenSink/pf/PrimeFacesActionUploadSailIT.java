package sail.kitchenSink.pf;

import org.junit.jupiter.api.Test;

import util.AbstractInterpretedSailWorkflow;

/**
 * An action upload that succeeds closes its upload page, which refreshes the view, and shows the page's messages in the
 * view's growl: the upload's success and the upload action's own growl,
 * in each PrimeFaces UX/UI: an overlay for {@code tablet} and {@code external}, a full-screen dialog for {@code phone}.
 * The messages and pushed growls are added to the growls showing: a growl pushed just before them, or after them, does
 * not remove them, nor they it.
 * <p>
 * The upload step closes an upload page that stays open with its close icon, as a user would, for a Skyve without the
 * fix. So the script first hides the close icons: an upload page that does not close itself then fails the step.
 */
class PrimeFacesActionUploadSailIT extends AbstractInterpretedSailWorkflow {
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
				"<comment>A growl is pushed just before the page shows its messages: neither removes the other</comment>" +
				"<execute>var after = SKYVE.PF.afterActionUpload;" +
				"SKYVE.PF.afterActionUpload = function() {" +
				"  SKYVE.PF.onPushMessage([{type: 'g', severity: 'info', message: 'SAIL job finished'}]);" +
				"  return after.apply(this, arguments);" +
				"};</execute>" +
				"<upload action=\"ActionsUpload\" file=\"sail/files/sail-upload.txt\" />" +
				"<comment>The upload page's messages, in the view's growl</comment>" +
				"<testMessage contains=\"SAIL job finished\" type=\"info\" />" +
				"<testMessage contains=\"sail-upload.txt is uploaded\" type=\"info\" />" +
				"<testMessage contains=\"Actions Upload took the file\" type=\"info\" />" +
				"<comment>A growl pushed after them does not remove them</comment>" +
				"<execute>SKYVE.PF.onPushMessage([{type: 'g', severity: 'info', message: 'SAIL job pushed later'}]);</execute>" +
				"<testMessage contains=\"SAIL job pushed later\" type=\"info\" />" +
				"<testMessage contains=\"sail-upload.txt is uploaded\" type=\"info\" />" +
				"<testValue binding=\"outcome\" value=\"Actions Upload ran\" />" +
				"<comment>On a phone the growls cover the toolbar until they fade: close them, as a user would</comment>" +
				"<execute>PF('pushGrowl').removeAll();</execute>" +
				"<cancel />" +
				"</method></interaction></automation>";
	}
}
