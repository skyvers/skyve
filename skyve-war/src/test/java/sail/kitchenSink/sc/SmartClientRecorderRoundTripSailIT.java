package sail.kitchenSink.sc;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
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
		String xml = recordWorkflow(() -> sailResource("sail/kitchenSink/sc/sc-recorder-round-trip.xml"));
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

	/**
	 * Data grids, lookups, uploads, list filters, sorting and selection, a generated number reused later (a capture),
	 * server-side questions, and tree, map and calendar menu items: see sc-recorder-round-trip-more.xml.
	 */
	@Test
	void moreGestures() {
		String xml = recordWorkflow(() -> sailResource("sail/kitchenSink/sc/sc-recorder-round-trip-more.xml"));
		assertContainsAll(xml,
				"<captureValue binding=\"number\" variable=\"number\"/>",
				"<listGridFilter column=\"number\" value=\"{var:number}\" operator=\"equals\"/>",
				"<listGridFilter column=\"number\"/>",
				"<listGridSort column=\"description\" descending=\"true\"/>",
				"<listGridClearFilter/>",
				"<listGridSelect module=\"kitchensink\" query=\"qListGridFixtureVue\" rowContaining=\"0 SAIL lookup ",
				"<lookupDescriptionPick binding=\"selection\" rowContaining=\"0 SAIL lookup ",
				"<lookupDescriptionAutoComplete binding=\"singleSelection\" search=\"0 SAIL lookup ",
				"<lookupDescriptionEdit binding=\"lookupDescription\"/>",
				"<lookupDescriptionNew binding=\"lookupDescription\"/>",
				"<dataGridNew binding=\"inlineGrid\"/>",
				"<dataEnter binding=\"date\" value=\"07-Nov-2025\"/>",
				"<dataGridEdit binding=\"inlineGrid\" rowContaining=\"07-Nov-2025\"/>",
				"<dataGridRemove binding=\"inlineGrid\"",
				"<dataGridZoom binding=\"orderedGrid\" rowContaining=\"01-Jan-2020\"/>",
				"<dataGridSelect binding=\"orderedGrid\" rowContaining=\"01-Jan-2020\"/>",
				"<dataGridMove binding=\"rows\" rowContaining=\"Second ",
				"<upload action=\"ActionsUpload\" file=\"sail/files/sail-refused.txt\" testSuccess=\"false\"/>",
				"<upload binding=\"contentLinkNone\" file=\"sail/files/sail-upload.txt\"/>",
				"text=\"Run From Row\" confirm=\"false\"/>",
				"<action name=\"AskYesNo\" confirm=\"false\"/>",
				"<navigateTree module=\"kitchensink\" query=\"qConditionsFixtureNodes\"/>",
				"<navigateMap module=\"kitchensink\" document=\"ListAttributes\" geometryBinding=\"geometry\"/>",
				"<navigateCalendar module=\"kitchensink\" document=\"ListAttributes\"/>");
		replay(xml);
	}

	@Test
	void adminContacts() {
		String xml = recordWorkflow(() -> sailString(CONTACTS));
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
	 * Checks, notes, a delete and an undo through the recorder's panel, with real gestures: a Check mode click on a
	 * field's validation error (a message) and on a grid row, an alt-click on a field and on a grid row, two notes typed into the panel, the delete of
	 * one note and the undo of one check. The checks must be in the script where they were added, and must pass when it
	 * is replayed unedited.
	 */
	@Test
	void checksThroughThePanel() {
		// one value across the scripts below ({run} differs in each), so that the row clicked is this test's own
		String value = "SAIL checked " + Long.toString(System.currentTimeMillis(), 36);
		sailString(SIGN_IN);
		sailString(START);
		awaitRecorder();

		// a save that validation refuses shows the error on the field: check it in Check mode
		sailString(HEAD + "<interaction name=\"New lookup value\"><method>" +
					"<navigateMenu module=\"kitchensink\" item=\"Lookup values\" />" +
					"<listGridNew module=\"kitchensink\" document=\"LookupDescription\" />" +
					"<save testSuccess=\"false\" />" +
					"<testFailure />" +
					"</method></interaction>" + TAIL);
		panel("check");
		click(field("description"), false);
		awaitRecorder();
		new Actions(driver()).sendKeys(Keys.ESCAPE).perform();
		assertNotEquals(Boolean.TRUE, js("return SKYVE.SailRecorder.checking();"), "Esc leaves Check mode");

		// enter and save the value, then alt-click its field to check it
		sailString(HEAD + "<interaction name=\"Lookup value\"><method>" +
					"<pushEditContext module=\"kitchensink\" document=\"LookupDescription\" />" +
					"<dataEnter binding=\"description\" value=\"" + value + "\" />" +
					"<save />" +
					"</method></interaction>" + TAIL);
		click(field("description"), true);
		awaitRecorder();
		sailString(HEAD + "<interaction name=\"Back to the list\"><method>" +
					"<pushEditContext module=\"kitchensink\" document=\"LookupDescription\" />" +
					"<ok />" +
					"</method></interaction>" + TAIL);

		// check the row in Check mode
		panel("check");
		click(row(value), false);
		awaitRecorder();
		new Actions(driver()).sendKeys(Keys.ESCAPE).perform();

		// two notes, one deleted
		note("A note from the panel");
		note("Delete me");
		panel("expand");
		selenium.waitFor(d -> Boolean.TRUE.equals(js("return Array.prototype.some.call(document.getElementById('skyve-sail-recorder').shadowRoot.querySelectorAll('.entry'), " +
														"function (e) { return e.textContent.indexOf('Delete me') >= 0; });")) ? Boolean.TRUE : null);
		click(element("var found = null; Array.prototype.forEach.call(document.getElementById('skyve-sail-recorder').shadowRoot.querySelectorAll('.entry'), " +
						"function (e) { if (e.textContent.indexOf('Delete me') >= 0) { found = e.querySelector('button[data-act=delete]'); } }); return found;"), false);
		awaitPanel();

		// a second row check by alt-click, undone
		click(row(value), true);
		awaitRecorder();
		panel("undo");
		awaitPanel();
		panel("collapse");
		// leave no lookup value behind
		sailString(HEAD + "<interaction name=\"Delete the lookup value\"><method>" +
					"<pushListContext module=\"kitchensink\" document=\"LookupDescription\" />" +
					"<listGridZoom module=\"kitchensink\" document=\"LookupDescription\" rowContaining=\"" + value + "\" />" +
					"<delete />" +
					"</method></interaction>" + TAIL);
		awaitRecorder();

		// stop in the panel, which offers the download
		panel("stop");
		selenium.waitFor(d -> Boolean.TRUE.equals(js("return !SKYVE.SailRecorder.active() && !SKYVE.SailRecorder.busy();")) ? Boolean.TRUE : null);
		assertEquals(Boolean.TRUE, js("return !document.getElementById('skyve-sail-recorder').shadowRoot.querySelector('.bar [data-act=download]').hidden;"),
					"Stop offers the download");

		String xml = fetch("sailrecording?format=xml");
		selenium.trace("Recorded:\n" + xml);
		assertAll(() -> assertTrue(xml.contains("<save testSuccess=\"false\"/>"), xml),
					() -> assertTrue(xml.contains("<testMessage contains=\""), xml),
					() -> assertTrue(xml.contains("<testValue binding=\"description\" value=\"" + value + "\"/>"), xml),
					() -> assertTrue(xml.contains("<testGridRows rowContaining=\"" + value + "\""), xml),
					() -> assertEquals(1, xml.split("<testGridRows ", -1).length - 1, "the undone check is gone: " + xml),
					() -> assertTrue(xml.contains("A note from the panel"), xml),
					() -> assertFalse(xml.contains("Delete me"), xml),
					() -> assertFalse(xml.contains("unmapped"), xml),
					() -> assertFalse(xml.contains("unconfirmed"), xml));
		replay(xml);
	}

	/**
	 * OrderedGridFixture's ordered list membership, with real gestures: members assigned with its button (the executor's
	 * dataEnter) and saved, a candidate dragged into the members at the top, and a member dragged to the top (the
	 * executor's listMembershipMove); then a save, and once the fixture is reopened from its list, an alt-click on the
	 * members to check their order. The script must have the members as dataEnter (the dragged candidate's dataEnter adds
	 * it last, so a listMembershipMove puts it at the top), each drag as listMembershipMove and the order as testValue,
	 * so that its replay, unedited, saves and checks the same order.
	 */
	@Test
	void orderedMembership() {
		// one name across the scripts below ({run} differs in each)
		String name = "SAIL members " + Long.toString(System.currentTimeMillis(), 36);
		String a = name + " a";
		String b = name + " b";
		String c = name + " c";
		String xml = recordWorkflow(() -> {
			StringBuilder lookups = new StringBuilder(512);
			for (String description : new String[] {a, b, c}) {
				lookups.append("<listGridNew module=\"kitchensink\" document=\"LookupDescription\" />")
						.append("<dataEnter binding=\"description\" value=\"").append(description).append("\" />")
						.append("<save /><ok />");
			}
			sailString(HEAD + "<interaction name=\"Lookup values, then a fixture with members\"><method>" +
						"<navigateMenu module=\"kitchensink\" item=\"Lookup values\" />" + lookups +
						"<navigateMenu module=\"kitchensink\" item=\"Ordered Grid Fixtures\" />" +
						"<listGridNew module=\"kitchensink\" document=\"OrderedGridFixture\" />" +
						"<dataEnter binding=\"name\" value=\"" + name + "\" />" +
						"<dataEnter binding=\"members\" value=\"" + a + "|" + b + "\" />" +
						"<save />" +
						"</method></interaction>" + TAIL);
			dragCandidate("members", c, 0);
			sailString(HEAD + "<interaction name=\"Reorder, save and reopen\"><method>" +
						"<pushEditContext module=\"kitchensink\" document=\"OrderedGridFixture\" />" +
						"<listMembershipMove binding=\"members\" rowContaining=\"" + b + "\" to=\"0\" />" +
						"<testValue binding=\"members\" value=\"" + b + "|" + c + "|" + a + "\" />" +
						"<save />" +
						"<ok />" +
						"<pushListContext module=\"kitchensink\" document=\"OrderedGridFixture\" />" +
						"<listGridZoom module=\"kitchensink\" document=\"OrderedGridFixture\" rowContaining=\"" + name + "\" />" +
						"</method></interaction>" + TAIL);
			click(element(MEMBERSHIP + "return m ? m._memberList.getHandle() : null;"), true);
			awaitRecorder();
			StringBuilder deletes = new StringBuilder(512);
			for (String description : new String[] {a, b, c}) {
				deletes.append("<listGridZoom module=\"kitchensink\" document=\"LookupDescription\" rowContaining=\"").append(description).append("\" />")
						.append("<delete />");
			}
			sailString(HEAD + "<interaction name=\"Delete the fixture and the lookup values\"><method>" +
						"<pushEditContext module=\"kitchensink\" document=\"OrderedGridFixture\" />" +
						"<delete />" +
						"<navigateMenu module=\"kitchensink\" item=\"Lookup values\" />" + deletes +
						"</method></interaction>" + TAIL);
		});
		assertContainsAll(xml,
				"<dataEnter binding=\"members\" value=\"" + a + "|" + b + "\"/>",
				"<dataEnter binding=\"members\" value=\"" + c + "|" + a + "|" + b + "\"/>",
				"<listMembershipMove binding=\"members\" rowContaining=\"" + c + "\" to=\"0\"/>",
				"<listMembershipMove binding=\"members\" rowContaining=\"" + b + "\" to=\"0\"/>",
				"<testValue binding=\"members\" value=\"" + b + "|" + c + "|" + a + "\"/>");
		replay(xml);
	}

	/** Finds the top view's list membership as {@code m} (or null), for a script that goes on to use it. */
	private static final String MEMBERSHIP = "var m = null; isc.Canvas._canvasList.forEach(function (x) {" +
												"if (x && !x.destroyed && isc.isA.BizListMembership(x) && (x._b === 'members') && x.isDrawn() && x.isVisible()) { m = x; } });";

	/**
	 * Drags a candidate of a list membership onto the upper half of the member at a position, with a real pointer drag,
	 * as a user puts a new member there.
	 */
	private void dragCandidate(@Nonnull String binding, @Nonnull String text, int to) {
		String membership = MEMBERSHIP.replace("'members'", "'" + binding + "'");
		WebElement source = element(membership + "if (!m) return null; var l = m._candidateList; var d = l.getData() || [];" +
									"for (var i = 0; i < d.length; i++) { if (String(d[i].bizKey).indexOf('" + text + "') >= 0) { l.scrollToRow(i);" +
									"return isc.AutoTest.getElement(isc.AutoTest.getObjectLocator(l) + '/body/row[' + i + ']/col[0]'); } } return null;");
		WebElement target = element(membership + "return m ? isc.AutoTest.getElement(isc.AutoTest.getObjectLocator(m._memberList) + '/body/row[" + to + "]/col[0]') : null;");
		int quarter = Math.max(1, target.getSize().getHeight() / 4);
		// press on the candidate, move past SmartClient's drag threshold, then over the upper half of the member, and release
		new Actions(driver()).moveToElement(source)
								.clickAndHold()
								.moveByOffset(5, 0)
								.moveByOffset(5, 0)
								.moveToElement(target, 0, -quarter)
								.moveByOffset(0, -1)
								.release()
								.perform();
		// SmartClient moves the record when the drop is handled: wait for the member list to show it at the position
		element(membership + "var r = m && m._memberList.getRecord(" + to + ");" +
				"return (r && (String(r.bizKey).indexOf('" + text + "') >= 0)) ? m._memberList.getHandle() : null;");
	}

	/**
	 * Clicks a recorder panel button (in its Shadow DOM) with a real click.
	 */
	private void panel(@Nonnull String act) {
		WebElement button = element("var root = document.getElementById('skyve-sail-recorder').shadowRoot;" +
									"return root.querySelector('.bar [data-act=\"" + act + "\"]') || root.querySelector('.drawer [data-act=\"" + act + "\"]');");
		click(button, false);
		awaitPanel();
	}

	/**
	 * Adds a note through the panel: Note, then the text and Enter in its input.
	 */
	private void note(@Nonnull String text) {
		panel("note");
		WebElement input = element("return document.getElementById('skyve-sail-recorder').shadowRoot.querySelector('[data-act=note-text]');");
		input.sendKeys(text + Keys.ENTER);
		awaitRecorder();
	}

	/**
	 * Clicks an element with a real pointer gesture, with Alt held for an alt-click.
	 */
	private void click(@Nonnull WebElement element, boolean alt) {
		Actions actions = new Actions(driver());
		if (alt) {
			actions.keyDown(Keys.ALT).moveToElement(element).click().keyUp(Keys.ALT).perform();
		}
		else {
			actions.moveToElement(element).click().perform();
		}
	}

	/**
	 * The input of a field of the view on top.
	 */
	private @Nonnull WebElement field(@Nonnull String name) {
		return element("var stack = isc.WindowStack._stack || []; var view = null;" +
						"for (var i = stack.length - 1; (i >= 0) && !view; i--) { var it = stack[i].items && stack[i].items[0]; if (it && it._vm) { view = it; } }" +
						"view = view || isc.BizUtil.getCurrentView(); var found = null;" +
						"isc.Canvas._canvasList.forEach(function (c) { if (c && !c.destroyed && (c._view === view) && isc.isA.DynamicForm(c) && c.getItem('" + name + "')) {" +
						"var item = c.getItem('" + name + "'); found = item.getDataElement() || item.getFocusElement(); } });" +
						"return found;");
	}

	/**
	 * The first cell of the first row of the list view's grid that shows the text.
	 */
	private @Nonnull WebElement row(@Nonnull String text) {
		return element("var g = isc.ListView._grid.grid; var n = g.getTotalRows();" +
						"for (var r = 0; r < n; r++) { var rec = g.getRecord(r); if (rec) { var f = g.getFields();" +
						"for (var c = 0; c < f.length; c++) { if (String(g.getCellValue(rec, r, c)).indexOf('" + text + "') >= 0) {" +
						"return isc.AutoTest.getElement(isc.AutoTest.getObjectLocator(g) + '/body/row[' + r + ']/col[' + c + ']'); } } } }" +
						"return null;");
	}

	/**
	 * Waits for a script to find an element: the panel and the page are drawn as responses arrive.
	 */
	private @Nonnull WebElement element(@Nonnull String script) {
		return selenium.waitFor(d -> (((JavascriptExecutor) d).executeScript(script) instanceof WebElement element) ? element : null);
	}

	private @Nullable Object js(@Nonnull String script) {
		return ((JavascriptExecutor) driver()).executeScript(script);
	}

	private @Nonnull WebDriver driver() {
		return selenium.waitFor(d -> d);
	}

	/**
	 * Waits for the panel's requests (a delete, an undo, the script it shows) to return.
	 */
	private void awaitPanel() {
		selenium.waitFor(d -> Boolean.TRUE.equals(((JavascriptExecutor) d).executeScript(
				"return !!(window.SKYVE && SKYVE.SailRecorder && !SKYVE.SailRecorder.busy());")) ? Boolean.TRUE : null);
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
	private @Nonnull String recordWorkflow(@Nonnull Runnable workflow) {
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

	/** Asserts that the script has each of the texts, reporting every one it lacks. */
	private static void assertContainsAll(@Nonnull String xml, @Nonnull String... expected) {
		assertAll(java.util.Arrays.stream(expected).map(text -> (org.junit.jupiter.api.function.Executable) () -> assertTrue(xml.contains(text), "no " + text + " in " + xml)));
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
