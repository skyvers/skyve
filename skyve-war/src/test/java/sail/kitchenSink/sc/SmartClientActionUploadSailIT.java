package sail.kitchenSink.sc;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.codeborne.selenide.Selenide;

import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * A SmartClient action upload that succeeds closes its upload window, refreshes the view and shows the upload page's
 * messages in the view's growl (runtime issue P3, docs/sail-runtime-issues.md): the upload's success and the upload
 * action's own growl; one with problems leaves the window open, showing them. A BizPort import does the same: a clean
 * import closes its window, showing its success in the view, and one with warnings stays open.
 * <p>
 * The upload step closes an upload window that stays open with its close button, as a user would, for a Skyve without
 * the fix. So before the successful upload an {@code <execute>} step has windows open without a close button: an
 * upload window that does not close itself then fails the step. Before that, a refused upload's window must still be
 * open when the step reads the refusal, then the step closes it.
 * <p>
 * An upload step that fails does not say the window lost the OS focus for the focus moving into the upload page's frame.
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), headless unless {@code -Dsail.headless=false}.
 */
class SmartClientActionUploadSailIT extends AbstractSmartClientInterpretedSail {

	/** Has the windows opened from now on show no close button. */
	private static final String NO_CLOSE_BUTTONS = "var popup = isc.WindowStack.popup;" +
													"isc.WindowStack.popup = function(fromRect, title, showCloseButton) {" +
													"  var args = Array.prototype.slice.call(arguments);" +
													"  args[2] = false;" +
													"  return popup.apply(isc.WindowStack, args);" +
													"};";

	/**
	 * Hands the view a pushed growl, as the push stream does, when the upload page refreshes it: the pushed growl is
	 * showing, unread, when the upload's messages are shown.
	 */
	private static final String PUSH_ON_REFRESH = "var v = isc.BizUtil.getCurrentView();" +
													"var save = v.saveInstance;" +
													"v.saveInstance = function(validate, action) {" +
													"  if (action === '_PUSH') {" +
													"    isc.BizUtil.onPushMessage([{type: 'g', severity: 'info', message: 'SAIL job finished'}]);" +
													"  }" +
													"  return save.apply(this, arguments);" +
													"};";

	/**
	 * Has every response of the view show a growl, as a response does when a bizlet or interceptor growls: the refresh
	 * after the upload shows one, without keeping the growls showing.
	 */
	private static final String GROWL_ON_EACH_RESPONSE = "var v = isc.BizUtil.getCurrentView();" +
															"var display = v._displayGrowlsAndMessages;" +
															"v._displayGrowlsAndMessages = function(growls, messages) {" +
															"  return display.call(this, (growls || []).concat([{severity: 'info', summary: 'SAIL response growl'}]), messages);" +
															"};";

	/** Removes the growls showing, so that a check sees only what follows. */
	private static final String NO_GROWLS = "jQuery('.ui-growl-item-container').remove();";

	public SmartClientActionUploadSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome));
	}

	@Test
	void aSuccessfulActionUploadClosesItsWindow() {
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<before><login user=\"admin\" password=\"admin\" /><switchMode /></before>" +
							"<interaction name=\"Action upload\"><method>" +
							"<navigateEdit module=\"kitchensink\" document=\"ActionsFixture\" />" +
							"<comment>Refused: the window stays open showing why, and the step closes it</comment>" +
							"<upload action=\"ActionsUpload\" file=\"sail/files/sail-refused.txt\" testSuccess=\"false\" />" +
							"<testFailure message=\"sail-refused.txt already exists\" />" +
							"<comment>No close buttons: the upload window must close by itself</comment>" +
							"<execute>" + NO_CLOSE_BUTTONS + "</execute>" +
							"<upload action=\"ActionsUpload\" file=\"sail/files/sail-upload.txt\" />" +
							"<comment>The upload page's messages, in the view's growl</comment>" +
							"<testMessage contains=\"sail-upload.txt is uploaded\" type=\"info\" />" +
							"<testMessage contains=\"Actions Upload took the file\" type=\"info\" />" +
							"<testValue binding=\"outcome\" value=\"Actions Upload ran\" />" +
							"</method></interaction></automation>";
		Assertions.assertDoesNotThrow(() -> sailString(script));

		Assertions.assertEquals(Long.valueOf(0), Selenide.executeJavaScript("return isc.WindowStack._stack.length"));
	}

	/**
	 * The upload's messages are added to the growls showing: a growl pushed just before them is not removed.
	 */
	@Test
	void aGrowlPushedBeforeTheUploadsMessagesStaysShowing() {
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<before><login user=\"admin\" password=\"admin\" /><switchMode /></before>" +
							"<interaction name=\"A pushed growl, then the upload's\"><method>" +
							"<navigateEdit module=\"kitchensink\" document=\"ActionsFixture\" />" +
							"<execute>" + NO_GROWLS + PUSH_ON_REFRESH + "</execute>" +
							"<upload action=\"ActionsUpload\" file=\"sail/files/sail-upload.txt\" />" +
							"<testMessage contains=\"SAIL job finished\" type=\"info\" />" +
							"<testMessage contains=\"sail-upload.txt is uploaded\" type=\"info\" />" +
							"</method></interaction></automation>";
		Assertions.assertDoesNotThrow(() -> sailString(script));
	}

	/**
	 * The upload's messages are shown once the refresh has returned: a growl in the refresh's response, which is shown
	 * without keeping the growls showing, does not remove them.
	 */
	@Test
	void aGrowlInTheRefreshDoesNotRemoveTheUploadsMessages() {
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<before><login user=\"admin\" password=\"admin\" /><switchMode /></before>" +
							"<interaction name=\"A growl in the refresh's response, then the upload's\"><method>" +
							"<navigateEdit module=\"kitchensink\" document=\"ActionsFixture\" />" +
							"<execute>" + NO_GROWLS + GROWL_ON_EACH_RESPONSE + "</execute>" +
							"<upload action=\"ActionsUpload\" file=\"sail/files/sail-upload.txt\" />" +
							"<testMessage contains=\"SAIL response growl\" type=\"info\" />" +
							"<testMessage contains=\"sail-upload.txt is uploaded\" type=\"info\" />" +
							"</method></interaction></automation>";
		Assertions.assertDoesNotThrow(() -> sailString(script));
	}

	/**
	 * A BizPort import without problems closes its window and shows its success in the view's growl; one with warnings
	 * leaves its window open, showing them, and the step closes it.
	 */
	@Test
	void aCleanImportClosesItsWindow() {
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<before><login user=\"admin\" password=\"admin\" /><switchMode /></before>" +
							"<interaction name=\"Import with warnings\"><method>" +
							"<navigateEdit module=\"kitchensink\" document=\"ActionsFixture\" />" +
							"<comment>Warnings: the window stays open showing them, and the step closes it</comment>" +
							"<upload action=\"ActionsImport\" file=\"sail/files/sail-import.xls\" testSuccess=\"false\" />" +
							"<testFailure message=\"Actions Import ran\" />" +
							"</method></interaction>" +
							"<interaction name=\"Clean import\"><method>" +
							"<comment>An empty workbook: nothing to import, so no problems (SmartClient never settles on the" +
							" kitchen sink's ConditionsFixture, whose import is clean, so admin's Data Maintenance is used)</comment>" +
							"<navigateEdit module=\"admin\" document=\"DataMaintenance\" />" +
							"<comment>No close buttons: the import window must close by itself</comment>" +
							"<execute>" + NO_CLOSE_BUTTONS + "</execute>" +
							"<upload action=\"DataMaintenanceImportAction\" file=\"sail/files/sail-import.xls\" />" +
							"<testMessage contains=\"The import completed successfully.\" type=\"info\" />" +
							"</method></interaction></automation>";
		Assertions.assertDoesNotThrow(() -> sailString(script));

		Assertions.assertEquals(Long.valueOf(0), Selenide.executeJavaScript("return isc.WindowStack._stack.length"));
	}

	/**
	 * An upload step that fails after working in the upload page's frame does not say the window lost the OS focus:
	 * the focus moving into the frame blurs the page's window, but the browser window keeps the OS focus.
	 */
	@Test
	void aFailedUploadDoesNotSayTheWindowLostTheFocus() {
		String script = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Verify\" xmlns=\"http://www.skyve.org/xml/sail\">" +
							"<before><login user=\"admin\" password=\"admin\" /><switchMode /></before>" +
							"<interaction name=\"Refused upload\"><method>" +
							"<navigateEdit module=\"kitchensink\" document=\"ActionsFixture\" />" +
							"<upload action=\"ActionsUpload\" file=\"sail/files/sail-refused.txt\" />" +
							"</method></interaction></automation>";
		AssertionError e = Assertions.assertThrows(AssertionError.class, () -> sailString(script));
		String message = e.getMessage();
		Assertions.assertAll(() -> Assertions.assertTrue(message.contains("The upload failed: ") &&
																message.contains("sail-refused.txt already exists"), message),
								() -> Assertions.assertFalse(message.contains("OS focus"), message));
	}
}
