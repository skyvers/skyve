package sail.kitchenSink.sc;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.JavascriptExecutor;

import jakarta.annotation.Nonnull;
import util.AbstractSmartClientInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;

/**
 * The SAIL recorder's round trip in SmartClient: run a script with recording on, download the recording, and replay
 * it, unedited, through the same executor. The workflows use only the gestures recorder phase 1 maps.
 * <p>
 * The recording starts in a session that is already signed in, so its script signs in again with
 * {@code password="{var:password}"}: the recorder never records a password. The test defines the variable, as a test
 * of a recorded script does, and the script runs as downloaded.
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), whose {@code skyve.json} has
 * {@code sail.recording} on; headless unless {@code -Dsail.headless=false}.
 */
class SmartClientRecorderRoundTripSailIT extends AbstractSmartClientInterpretedSail {
	private static final String HEAD = "<automation uxui=\"desktop\" userAgentType=\"desktop\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">";
	private static final String TAIL = "</automation>";
	private static final String TO_CONTROL_PANEL = "<navigateMenu module=\"admin\" item=\"DevOps/Control Panel\" /><tabSelect path=\"SAIL\" />";

	/** Signs in, switches to SmartClient and opens the Control Panel's SAIL tab. */
	private static final String SIGN_IN = HEAD +
			"<before><login user=\"admin\" password=\"admin\" /><switchMode /></before>" +
			"<interaction name=\"Control Panel\"><method>" + TO_CONTROL_PANEL + "</method></interaction>" +
			TAIL;
	private static final String START = HEAD +
			"<interaction name=\"Start recording\"><method>" + TO_CONTROL_PANEL + "<action name=\"StartSAILRecording\" /></method></interaction>" +
			TAIL;
	private static final String STOP = HEAD +
			"<interaction name=\"Stop recording\"><method>" + TO_CONTROL_PANEL + "<action name=\"StopSAILRecording\" /></method></interaction>" +
			TAIL;

	/** Contacts: create, find, change and delete (declined, then confirmed), from the SmartClient admin suite. */
	private static final String CONTACTS = HEAD +
			"<interaction name=\"Contacts: create, find, change and delete\"><method>" +
			"<navigateMenu module=\"admin\" item=\"Contacts\" />" +
			"<listGridNew module=\"admin\" document=\"Contact\" />" +
			"<dataEnter binding=\"name\" value=\"SAIL Recorded Contact {run}\" />" +
			"<dataEnter binding=\"email1\" value=\"sail.recorded.{run}@example.com\" />" +
			"<dataEnter binding=\"mobile\" value=\"0400 111 222\" />" +
			"<save />" +
			"<testValue binding=\"name\" value=\"SAIL Recorded Contact {run}\" />" +
			"<ok />" +
			"<testGridRows module=\"admin\" document=\"Contact\" rowContaining=\"SAIL Recorded Contact {run}\" minRows=\"1\" maxRows=\"1\" />" +
			"<listGridZoom module=\"admin\" document=\"Contact\" rowContaining=\"SAIL Recorded Contact {run}\" />" +
			"<dataEnter binding=\"mobile\" value=\"0400 333 444\" />" +
			"<save />" +
			"<testValue binding=\"mobile\" value=\"0400 333 444\" />" +
			"<delete confirm=\"false\" />" +
			"<delete />" +
			"<testGridRows module=\"admin\" document=\"Contact\" rowContaining=\"SAIL Recorded Contact {run}\" maxRows=\"0\" />" +
			"</method></interaction>" +
			TAIL;

	/** The recorder's script tag, as the home page renders it while the session records. */
	private static final String RECORDER_TAG = java.util.regex.Pattern.quote("<script type=\"text/javascript\" src=\"skyve/sail/recorder-sc-min.js?v=") +
												"[^\"]*" + java.util.regex.Pattern.quote("\"></script>");

	public SmartClientRecorderRoundTripSailIT() {
		super(new BrowserConfiguration().browser(Browsers.chrome));
	}

	@Test
	void kitchenSink() {
		String xml = record(() -> sailResource("sail/kitchenSink/sc/sc-recorder-round-trip.xml"));
		assertAll(() -> assertTrue(xml.contains("<navigateMenu module=\"kitchensink\" item=\"Lookup values\"/>"), xml),
					() -> assertTrue(xml.contains("<navigateEdit module=\"kitchensink\" document=\"KitchenSink\"/>"), xml),
					() -> assertTrue(xml.contains("<dataEnter binding=\"combo\" value=\"one\" match=\"code\"/>"), xml),
					() -> assertTrue(xml.contains("<dataEnter binding=\"memo\" value=\"First line&#10;Second line\"/>"), xml),
					() -> assertTrue(xml.contains("<tabSelect path=\"Rows\"/>"), xml),
					() -> assertTrue(xml.contains("<tabSelect path=\"Tab button\"/>"), xml),
					() -> assertTrue(xml.contains("<action name=\"RunFromTabButton\"/>"), xml),
					() -> assertTrue(xml.contains("<!-- changed on screen: outcome "), xml),
					() -> assertTrue(xml.contains("<listGridZoom module=\"kitchensink\" query=\"qModelLookupFixtures\" rowContaining=\"SAIL recorded lookup "), xml));
		replay(xml);
	}

	@Test
	void adminContacts() {
		String xml = record(() -> sailString(CONTACTS));
		assertAll(() -> assertTrue(xml.contains("<listGridNew/>"), xml),
					() -> assertTrue(xml.contains("<listGridZoom rowContaining=\"SAIL Recorded Contact "), xml),
					() -> assertTrue(xml.contains("<delete confirm=\"false\"/>"), xml),
					() -> assertTrue(xml.contains("<delete/>"), xml),
					() -> assertTrue(xml.contains("<!-- screen: admin.Contact (Contact), "), xml),
					() -> assertTrue(xml.contains("<!-- \"Mobile\" was \"0400 111 222\" -->"), xml),
					() -> assertTrue(xml.contains("<!-- row: "), xml));
		replay(xml);
	}

	/**
	 * A page of a session that is not recording has no trace of the recorder: the home page before recording starts
	 * and after it stops is the page while it records without the recorder's script tag, and is otherwise the same.
	 */
	@Test
	void pagesAreUnchangedUnlessRecording() {
		sailString(SIGN_IN);
		String before = page();
		sailString(START);
		awaitRecorder();
		String during = page();
		sailString(STOP);
		String after = page();
		assertAll(() -> assertFalse(before.contains("sail/recorder"), before),
					() -> assertTrue(during.matches("(?s).*" + RECORDER_TAG + ".*"), during),
					() -> assertEquals(before, during.replaceAll(RECORDER_TAG, "")),
					() -> assertEquals(before, after));
	}

	/**
	 * Signs in, starts recording in the Control Panel, runs the workflow, stops recording and returns the script.
	 */
	private @Nonnull String record(@Nonnull Runnable workflow) {
		sailString(SIGN_IN);
		sailString(START);
		awaitRecorder();
		workflow.run();
		sailString(STOP);
		String xml = fetch("sailrecording?format=xml");
		selenium.trace("Recorded:\n" + xml);
		assertAll(() -> assertTrue(xml.startsWith("<?xml"), xml),
					() -> assertTrue(xml.contains("<login user=\"admin\" password=\"{var:password}\"/>"), xml),
					() -> assertFalse(xml.contains("unmapped"), xml),
					() -> assertFalse(xml.contains("StopSAILRecording"), xml),
					() -> assertFalse(xml.contains("item=\"DevOps/Control Panel\""), xml),
					() -> assertFalse(xml.contains("unconfirmed"), xml),
					() -> assertTrue(xml.contains("<!-- The recorded user's roles, for the test: viewRoles(\"admin\", \"admin."), xml),
					() -> assertTrue(xml.contains("<!-- The recorded user got the UX/UI desktop. -->"), xml));
		return xml;
	}

	/**
	 * Runs the recorded script as downloaded, defining only the password it signs in with.
	 */
	private void replay(@Nonnull String xml) {
		variable("password", "admin");
		sailString(xml);
	}

	/**
	 * Waits for the recorder that the Control Panel's Start loads into the page: its script loads, and it asks the
	 * server for its token, after the action's response, so the next gesture would otherwise go unrecorded.
	 */
	private void awaitRecorder() {
		selenium.waitFor(d -> Boolean.TRUE.equals(((JavascriptExecutor) d).executeScript(
				"return !!(window.SKYVE && SKYVE.SailRecorder && SKYVE.SailRecorder.ready());")) ? Boolean.TRUE : null);
	}

	/**
	 * Returns the kitchen sink's edit page, as the server renders it for this session.
	 */
	private @Nonnull String page() {
		return fetch("?a=e&m=kitchensink&d=KitchenSink")
				// a JSF view state differs on every render
				.replaceAll("(name=\"jakarta.faces.ViewState\"[^>]*value=\")[^\"]*\"", "$1\"");
	}

	/**
	 * Fetches a URL relative to the base URL in the browser's session and returns its text.
	 */
	private @Nonnull String fetch(@Nonnull String relativeUrl) {
		String url = new BrowserConfiguration().getBaseUrl() + relativeUrl;
		Object result = selenium.waitFor(d -> ((JavascriptExecutor) d).executeAsyncScript(
				"var done = arguments[arguments.length - 1];" +
				"fetch(arguments[0], {credentials: 'same-origin'}).then(function (r) { return r.text(); })" +
				".then(done, function (e) { done('fetch failed: ' + e); });", url));
		return String.valueOf(result);
	}
}
