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
