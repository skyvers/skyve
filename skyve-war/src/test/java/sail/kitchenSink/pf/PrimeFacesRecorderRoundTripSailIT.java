package sail.kitchenSink.pf;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

/**
 * The SAIL recorder's round trip in PrimeFaces (the tablet UX/UI): run a script with recording on, download the
 * recording, and replay it, unedited, through the PrimeFaces executor. The workflows use only the gestures the
 * PrimeFaces recorder maps; one adds checks, notes, a delete and an undo through the recorder's panel.
 * <p>
 * The recording starts in a session that is already signed in, so its script signs in again with
 * {@code password="{var:password}"}, which the test defines.
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), whose {@code skyve.json} has
 * {@code sail.recording} on; headless unless {@code -Dsail.headless=false}.
 */
class PrimeFacesRecorderRoundTripSailIT extends AbstractPrimeFacesInterpretedSail {
	private static final String HEAD = "<automation uxui=\"tablet\" userAgentType=\"tablet\" testStrategy=\"Assert\" xmlns=\"http://www.skyve.org/xml/sail\">";
	private static final String TAIL = "</automation>";
	private static final String TO_CONTROL_PANEL = "<navigateMenu module=\"admin\" item=\"DevOps/Control Panel\" /><tabSelect path=\"SAIL\" />";

	/** Signs in and opens the Control Panel's SAIL tab. */
	private static final String SIGN_IN = HEAD +
			"<before><login user=\"admin\" password=\"admin\" /></before>" +
			"<interaction name=\"Control Panel\"><method>" + TO_CONTROL_PANEL + "</method></interaction>" +
			TAIL;
	/** Starts recording: in PrimeFaces, the page loads again with the recorder. */
	private static final String START = HEAD +
			"<interaction name=\"Start recording\"><method>" + TO_CONTROL_PANEL + "<action name=\"StartSAILRecording\" /></method></interaction>" +
			TAIL;
	private static final String STOP = HEAD +
			"<interaction name=\"Stop recording\"><method>" + TO_CONTROL_PANEL + "<action name=\"StopSAILRecording\" /></method></interaction>" +
			TAIL;

	/** Contacts: create, find, change and delete (declined, then confirmed), from the PrimeFaces admin suite. */
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

	/** What a page has only while its session records: the recorder's script tag, map and state. */
	private static final String RECORDER_TAG = Pattern.quote("<script type=\"text/javascript\" src=\"skyve/sail/recorder-pf-min.js?v=") +
												"[^\"]*" + Pattern.quote("\"></script>");
	private static final String RECORDER_SPANS = "<span id=\"sailRecorder(Map|State)\" hidden=\"hidden\" data-(map|state)=\"[^\"]*\"></span>";

	public PrimeFacesRecorderRoundTripSailIT() {
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	@Test
	void kitchenSink() {
		String xml = record(() -> sailResource("sail/kitchenSink/pf/pf-recorder-round-trip.xml"));
		assertAll(() -> assertTrue(xml.contains("<automation uxui=\"tablet\" userAgentType=\"tablet\""), xml),
					() -> assertTrue(xml.contains("<navigateMenu module=\"kitchensink\" item=\"Lookup values\"/>"), xml),
					() -> assertTrue(xml.contains("<listGridZoom rowContaining=\"0 SAIL Recorded "), xml),
					() -> assertTrue(xml.contains("<listGridZoom module=\"kitchensink\" query=\"qListGridFixtureVue\" rowContaining=\"0 SAIL Recorded "), xml),
					() -> assertTrue(xml.contains("<listGridZoom module=\"kitchensink\" query=\"qListGridFixtureFaces\" rowContaining=\"0 SAIL Recorded "), xml),
					() -> assertTrue(xml.contains("<dataEnter binding=\"booleanFlag\" value=\"true\"/>"), xml),
					() -> assertTrue(xml.contains("<navigateEdit module=\"kitchensink\" document=\"KitchenSink\"/>"), xml),
					() -> assertTrue(xml.contains("<dataEnter binding=\"combo\" value=\"one\" match=\"code\"/>"), xml),
					() -> assertTrue(xml.contains("<dataEnter binding=\"memo\" value=\"First line&#10;Second line\"/>"), xml),
					() -> assertTrue(xml.contains("<tabSelect path=\"Rows\"/>"), xml),
					() -> assertTrue(xml.contains("<action name=\"RunFromTabButton\"/>"), xml));
		replay(xml);
	}

	/**
	 * Lookups, data grids, uploads, Vue list grid filters, sorting and selection, a data table's selection, a generated
	 * number reused later (a capture) and server-side questions: see pf-recorder-round-trip-more.xml.
	 */
	@Test
	void moreGestures() {
		String xml = record(() -> sailResource("sail/kitchenSink/pf/pf-recorder-round-trip-more.xml"));
		assertAll(() -> assertTrue(xml.contains("<captureValue binding=\"number\" variable=\"number\"/>"), xml),
					() -> assertTrue(xml.contains("<listGridFilter column=\"number\" value=\"{var:number}\" operator=\"equals\"/>"), xml),
					() -> assertTrue(xml.contains("<listGridFilter column=\"number\"/>"), xml),
					() -> assertTrue(xml.contains("<listGridSort column=\"description\" descending=\"true\"/>"), xml),
					() -> assertTrue(xml.contains("<listGridClearFilter/>"), xml),
					() -> assertTrue(xml.contains("<listGridSelect module=\"kitchensink\" query=\"qListGridFixtureVue\" rowContaining=\"0 SAIL lookup "), xml),
					() -> assertTrue(xml.contains("<listGridSelect module=\"kitchensink\" query=\"qListGridFixtureFaces\" rowContaining=\"0 SAIL lookup "), xml),
					() -> assertTrue(xml.contains("<lookupDescriptionPick binding=\"lookupDescription\" rowContaining=\"0 SAIL lookup "), xml),
					() -> assertTrue(xml.contains("<lookupDescriptionAutoComplete binding=\"lookupDescription\" search=\"0 SAIL lookup "), xml),
					() -> assertTrue(xml.contains("<dataGridNew binding=\"containerGrid\"/>"), xml),
					() -> assertTrue(xml.contains("<dataGridZoom binding=\"containerGrid\""), xml),
					() -> assertTrue(xml.contains("<dataGridRemove binding=\"inlineGrid\""), xml),
					() -> assertTrue(xml.contains("<dataGridMove binding=\"rows\" rowContaining=\"Second "), xml),
					() -> assertTrue(xml.contains("<upload action=\"ActionsUpload\" file=\"sail/files/sail-refused.txt\" testSuccess=\"false\"/>"), xml),
					() -> assertTrue(xml.contains("<upload binding=\"contentLink\" file=\"sail/files/sail-upload.txt\"/>"), xml),
					() -> assertTrue(xml.contains("text=\"Run From Row\" confirm=\"false\"/>"), xml),
					() -> assertTrue(xml.contains("<action name=\"AskYesNo\" confirm=\"false\"/>"), xml));
		replay(xml);
	}

	@Test
	void adminContacts() {
		String xml = record(() -> sailString(CONTACTS));
		assertAll(() -> assertTrue(xml.contains("<listGridNew/>"), xml),
					() -> assertTrue(xml.contains("<listGridZoom rowContaining=\"SAIL Recorded Contact "), xml),
					() -> assertTrue(xml.contains("<delete confirm=\"false\"/>"), xml),
					() -> assertTrue(xml.contains("<delete/>"), xml));
		replay(xml);
	}

	/**
	 * Checks, notes, a delete and an undo through the recorder's panel, with real gestures: a Check mode click on a
	 * field's validation error and on a list row, an alt-click on a field and on a row, two notes, the delete of one note
	 * and the undo of one check, then Stop in the panel. The checks must be in the script and pass when it is replayed.
	 */
	@Test
	void checksThroughThePanel() {
		// one value across the scripts below ({run} differs in each), that sorts first in the list's first page of rows
		String value = "0 SAIL checked " + Long.toString(System.currentTimeMillis(), 36);
		sailString(SIGN_IN);
		sailString(START);
		awaitRecorder();

		sailString(HEAD + "<interaction name=\"New lookup value\"><method>" +
					"<navigateMenu module=\"kitchensink\" item=\"Lookup values\" />" +
					"<listGridNew module=\"kitchensink\" document=\"LookupDescription\" />" +
					"<save testSuccess=\"false\" />" +
					"<testFailure />" +
					"</method></interaction>" + TAIL);
		awaitRecorder();
		panel("check");
		click(field("description"), false);
		awaitRecorder();
		new Actions(driver()).sendKeys(Keys.ESCAPE).perform();
		assertFalse(Boolean.TRUE.equals(js("return SKYVE.SailRecorder.checking();")), "Esc leaves Check mode");

		sailString(HEAD + "<interaction name=\"Lookup value\"><method>" +
					"<pushEditContext module=\"kitchensink\" document=\"LookupDescription\" />" +
					"<dataEnter binding=\"description\" value=\"" + value + "\" />" +
					"<save />" +
					"</method></interaction>" + TAIL);
		awaitRecorder();
		click(field("description"), true);
		awaitRecorder();
		sailString(HEAD + "<interaction name=\"Back to the list\"><method>" +
					"<pushListContext module=\"kitchensink\" document=\"LookupDescription\" />" +
					"<pushEditContext module=\"kitchensink\" document=\"LookupDescription\" />" +
					"<ok />" +
					"</method></interaction>" + TAIL);
		awaitRecorder();

		panel("check");
		click(row(value), false);
		awaitRecorder();
		new Actions(driver()).sendKeys(Keys.ESCAPE).perform();

		note("A note from the panel");
		note("Delete me");
		panel("expand");
		selenium.waitFor(d -> Boolean.TRUE.equals(js("return Array.prototype.some.call(document.getElementById('skyve-sail-recorder').shadowRoot.querySelectorAll('.entry'), " +
														"function (e) { return e.textContent.indexOf('Delete me') >= 0; });")) ? Boolean.TRUE : null);
		click(element("var found = null; Array.prototype.forEach.call(document.getElementById('skyve-sail-recorder').shadowRoot.querySelectorAll('.entry'), " +
						"function (e) { if (e.textContent.indexOf('Delete me') >= 0) { found = e.querySelector('button[data-act=delete]'); } }); return found;"), false);
		awaitPanel();
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

		panel("stop");
		selenium.waitFor(d -> Boolean.TRUE.equals(js("return !SKYVE.SailRecorder.active() && !SKYVE.SailRecorder.busy();")) ? Boolean.TRUE : null);
		assertTrue(Boolean.TRUE.equals(js("return !document.getElementById('skyve-sail-recorder').shadowRoot.querySelector('.bar [data-act=download]').hidden;")),
					"Stop offers the download");

		String xml = fetch("sailrecording?format=xml");
		selenium.trace("Recorded:\n" + xml);
		assertAll(() -> assertTrue(xml.contains("<save testSuccess=\"false\"/>"), xml),
					() -> assertTrue(xml.contains("<testMessage contains=\""), xml),
					() -> assertTrue(xml.contains("<testValue binding=\"description\" value=\"0 SAIL checked "), xml),
					() -> assertTrue(xml.contains("<testGridRows rowContaining=\"0 SAIL checked "), xml),
					() -> assertEquals(1, xml.split("<testGridRows ", -1).length - 1, "the undone check is gone: " + xml),
					() -> assertTrue(xml.contains("A note from the panel"), xml),
					() -> assertFalse(xml.contains("Delete me"), xml),
					() -> assertFalse(xml.contains("unmapped"), xml),
					() -> assertFalse(xml.contains("unconfirmed"), xml));
		replay(xml);
	}

	/**
	 * Signing out while recording asks first: Keep recording stays on the page and records on; Stop and download stops
	 * the recording and leaves its script to download.
	 */
	@Test
	void signOutWarns() {
		sailString(SIGN_IN);
		sailString(START);
		awaitRecorder();
		String signOut = "var a = Array.prototype.find.call(document.querySelectorAll('a[href]'), function (e) {" +
							"return /(^|\\/)loggedOut$/.test(new URL(e.href, location.href).pathname); }); if (a) { a.click(); } return !!a;";
		String warningOpen = "return document.getElementById('skyve-sail-recorder').shadowRoot.querySelector('.warning').classList.contains('open');";
		assertTrue(Boolean.TRUE.equals(js(signOut)), "the page has a sign out link");
		assertTrue(Boolean.TRUE.equals(js(warningOpen)), "signing out warns");
		panel("keep");
		assertFalse(Boolean.TRUE.equals(js(warningOpen)), "Keep recording closes the warning");
		assertTrue(Boolean.TRUE.equals(js("return SKYVE.SailRecorder.active() && (location.pathname.indexOf('loggedOut') < 0);")), "still recording on the page");

		js(signOut);
		panel("stop-download");
		selenium.waitFor(d -> Boolean.TRUE.equals(js("return !SKYVE.SailRecorder.active() && !SKYVE.SailRecorder.busy();")) ? Boolean.TRUE : null);
		String xml = fetch("sailrecording?format=xml");
		assertTrue(xml.startsWith("<?xml") && xml.contains("<login user=\"admin\" password=\"{var:password}\"/>"), xml);
	}

	/**
	 * A page of a session that is not recording has no trace of the recorder: an edit page before recording starts and
	 * after it stops is the page while it records without the recorder's script tag, map and state.
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
		assertAll(() -> assertFalse(before.contains("sail/recorder") || before.contains("sailRecorder") || before.contains("xmlns:c"), before),
					() -> assertTrue(Pattern.compile(RECORDER_TAG).matcher(during).find(), during),
					() -> assertTrue(Pattern.compile(RECORDER_SPANS).matcher(during).find(), during),
					() -> assertEquals(before, during.replaceAll(RECORDER_TAG, "").replaceAll(RECORDER_SPANS, "")),
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
		awaitRecorder();
		sailString(STOP);
		String xml = fetch("sailrecording?format=xml");
		selenium.trace("Recorded:\n" + xml);
		assertAll(() -> assertTrue(xml.startsWith("<?xml"), xml),
					() -> assertTrue(xml.contains("<login user=\"admin\" password=\"{var:password}\"/>"), xml),
					() -> assertFalse(xml.contains("unmapped"), xml),
					() -> assertFalse(xml.contains("unconfirmed"), xml),
					() -> assertFalse(xml.contains("StopSAILRecording"), xml),
					() -> assertFalse(xml.contains("item=\"DevOps/Control Panel\""), xml));
		return xml;
	}

	private void replay(@Nonnull String xml) {
		variable("password", "admin");
		sailString(xml);
	}

	/**
	 * Waits for the recorder that each recording page loads to start and to have sent every step so far.
	 */
	private void awaitRecorder() {
		selenium.waitFor(d -> Boolean.TRUE.equals(((JavascriptExecutor) d).executeScript(
				"return !!(window.SKYVE && SKYVE.SailRecorder && SKYVE.SailRecorder.ready());")) ? Boolean.TRUE : null);
	}

	/**
	 * Waits for the panel's requests (a delete, an undo, the script it shows) to return.
	 */
	private void awaitPanel() {
		selenium.waitFor(d -> Boolean.TRUE.equals(((JavascriptExecutor) d).executeScript(
				"return !!(window.SKYVE && SKYVE.SailRecorder && !SKYVE.SailRecorder.busy());")) ? Boolean.TRUE : null);
	}

	private void panel(@Nonnull String act) {
		WebElement button = element("var root = document.getElementById('skyve-sail-recorder').shadowRoot;" +
									"return root.querySelector('.bar [data-act=\"" + act + "\"]') || root.querySelector('.drawer [data-act=\"" + act + "\"]') ||" +
									"root.querySelector('.warning [data-act=\"" + act + "\"]');");
		click(button, false);
		awaitPanel();
	}

	private void note(@Nonnull String text) {
		panel("note");
		WebElement input = element("return document.getElementById('skyve-sail-recorder').shadowRoot.querySelector('[data-act=note-text]');");
		input.sendKeys(text + Keys.ENTER);
		awaitRecorder();
	}

	private void click(@Nonnull WebElement element, boolean alt) {
		((JavascriptExecutor) driver()).executeScript("arguments[0].scrollIntoView({block: 'center'});", element);
		Actions actions = new Actions(driver());
		if (alt) {
			actions.keyDown(Keys.ALT).moveToElement(element).click().keyUp(Keys.ALT).perform();
		}
		else {
			actions.moveToElement(element).click().perform();
		}
	}

	/**
	 * The input of a field, found through the page's client-id map.
	 */
	private @Nonnull WebElement field(@Nonnull String binding) {
		return element("var m = document.getElementById('sailRecorderMap'); if (! m) return null; var ids = JSON.parse(m.getAttribute('data-map')).ids;" +
						"for (var id in ids) { if ((ids[id].binding === '" + binding + "') && ! ids[id].step) { var e = document.getElementById(id);" +
						"if (e) { return (e.matches('input, textarea') ? e : e.querySelector('input:not([type=hidden]), textarea')) || e; } } }" +
						"return null;");
	}

	/**
	 * The first cell of the first list row that shows the text.
	 */
	private @Nonnull WebElement row(@Nonnull String text) {
		return element("var rows = document.querySelectorAll('tbody.p-datatable-tbody > tr, tbody.ui-datatable-data > tr');" +
						"for (var i = 0; i < rows.length; i++) { var cells = rows[i].querySelectorAll('td');" +
						"for (var c = 0; c < cells.length; c++) { if ((cells[c].textContent || '').indexOf('" + text + "') >= 0) { return cells[c]; } } }" +
						"return null;");
	}

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
	 * Returns the kitchen sink's edit page, as the server renders it for this session, without what changes on every
	 * render: the Faces view state, the CSRF tokens and the conversation's id in the content upload URLs.
	 */
	private @Nonnull String page() {
		return fetch("?a=e&m=kitchensink&d=KitchenSink")
				.replaceAll("(name=\"jakarta.faces.ViewState\"[^>]*value=\")[^\"]*\"", "$1\"")
				.replaceAll("(id=\"[^\"]*ViewState[^\"]*\"[^>]*value=\")[^\"]*\"", "$1\"")
				.replaceAll("(name=\"(_csrf|uxuicsrf|csrfToken)\"[^>]*value=\")[^\"]*\"", "$1\"")
				.replaceAll("_c=[0-9a-f-]+", "_c=");
	}

	private @Nonnull String fetch(@Nonnull String relativeUrl) {
		String url = new BrowserConfiguration().getBaseUrl() + relativeUrl;
		Object result = selenium.waitFor(d -> ((JavascriptExecutor) d).executeAsyncScript(
				"var done = arguments[arguments.length - 1];" +
				"fetch(arguments[0], {credentials: 'same-origin'}).then(function (r) { return r.text(); })" +
				".then(done, function (e) { done('fetch failed: ' + e); });", url));
		return String.valueOf(result);
	}
}
