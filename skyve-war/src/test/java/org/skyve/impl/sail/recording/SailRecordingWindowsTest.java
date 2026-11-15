package org.skyve.impl.sail.recording;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

/**
 * Tests how the window, back and refresh steps are made from what the recorded pages said: each entry's window, a
 * page load's kind, a new window's first page and a window's "in front again" marker.
 */
@SuppressWarnings("static-method")
class SailRecordingWindowsTest {
	private static final String LINK_URL = "http://host/skyve/?a=e&m=kitchensink&d=LookupDescription";
	private static final String FIXTURE_URL = "http://host/skyve/?a=e&m=kitchensink&d=ListGridFixture";

	private static Map<String, String> step(String... keysAndValues) {
		Map<String, String> result = new LinkedHashMap<>();
		for (int i = 0; i < keysAndValues.length; i += 2) {
			result.put(keysAndValues[i], keysAndValues[i + 1]);
		}
		return result;
	}

	/** The entries the pass makes, flattened, as "op" or "op attribute=value" for the attributes that matter here. */
	private static List<String> ops(List<Map<String, String>> entries) {
		List<String> result = new ArrayList<>();
		for (List<Map<String, String>> group : SailRecordingWindows.grouped(entries)) {
			for (Map<String, String> entry : group) {
				StringBuilder op = new StringBuilder(entry.get("op"));
				for (String key : List.of("text", "href", "document", "module")) {
					if (entry.get(key) != null) {
						op.append(' ').append(key).append('=').append(entry.get(key));
					}
				}
				result.add(op.toString());
			}
		}
		return result;
	}

	@Test
	void aLinkIntoANewWindowItsPageAndTheWayBackAreTheWindowSteps() {
		List<String> result = ops(List.of(
				step("op", "navigateEdit", "module", "kitchensink", "document", "ListGridFixture", "windowId", "w0"),
				step("op", "navigateLink", "text", "Open", "window", "new", "url", LINK_URL, "windowId", "w0"),
				step("op", "navigateEdit", "module", "kitchensink", "document", "LookupDescription", "windowId", "w1",
						"opened", "true", "url", LINK_URL, "pageHref", "?a=e&m=kitchensink&d=LookupDescription"),
				step("op", "dataEnter", "binding", "description", "value", "x", "windowId", "w1"),
				step("op", "context", "focus", "true", "windowId", "w0"),
				step("op", "save", "windowId", "w0")));
		assertEquals(List.of("navigateEdit document=ListGridFixture module=kitchensink",
								"navigateLink text=Open",
								"pushEditContext document=LookupDescription module=kitchensink",
								"dataEnter",
								"closeWindow",
								"save"),
						result);
	}

	@Test
	void aWindowNoRecordedLinkOpenedIsOpenedByItsUrlAndNestedWindowsCloseTogether() {
		List<String> result = ops(List.of(
				step("op", "navigateLink", "text", "Open", "window", "new", "url", LINK_URL, "windowId", "w0"),
				step("op", "navigateEdit", "module", "kitchensink", "document", "LookupDescription", "windowId", "w1",
						"opened", "true", "url", LINK_URL),
				step("op", "navigateList", "module", "kitchensink", "document", "LookupDescription", "windowId", "w2",
						"opened", "true", "url", "http://host/skyve/?a=l&m=kitchensink&q=LookupDescription",
						"pageHref", "?a=l&m=kitchensink&q=LookupDescription"),
				step("op", "listGridNew", "windowId", "w2"),
				step("op", "save", "windowId", "w0")));
		assertEquals(List.of("navigateLink text=Open",
								"pushEditContext document=LookupDescription module=kitchensink",
								"navigateLink href=?a=l&m=kitchensink&q=LookupDescription",
								"pushListContext document=LookupDescription module=kitchensink",
								"listGridNew",
								"closeWindow",
								"closeWindow",
								"save"),
						result);
	}

	@Test
	void aWindowThatNeverRecordsClosesWhenAStepComesFromItsOpener() {
		List<String> result = ops(List.of(
				step("op", "navigateLink", "text", "PDF", "window", "new", "url", "http://host/skyve/resources?_n=a.pdf", "windowId", "w0"),
				step("op", "listGridTool", "tool", "refresh", "windowId", "w0")));
		assertEquals(List.of("navigateLink text=PDF", "closeWindow", "listGridTool"), result);
	}

	@Test
	void aReloadAndBackAreRefreshAndNavigateBackWithTheirPagesContextForwardIsUnmapped() {
		List<Map<String, String>> entries = List.of(
				step("op", "navigateEdit", "module", "m", "document", "D", "bizId", "1", "windowId", "w0", "load", "reload"),
				step("op", "navigateList", "module", "m", "query", "q", "windowId", "w0", "load", "back"),
				step("op", "context", "view", "", "windowId", "w0", "load", "reload"),
				step("op", "navigateEdit", "module", "m", "document", "D", "windowId", "w0", "load", "forward"));
		List<String> result = ops(entries);
		Map<String, String> push = SailRecordingWindows.grouped(entries).get(0).get(1);
		assertAll(() -> assertEquals(List.of("refresh", "pushEditContext document=D module=m",
												"navigateBack", "pushListContext module=m",
												"refresh",
												"unmapped text=went forward with the browser's Forward button, which SAIL has no step for: the page is opened by its URL instead",
												"navigateEdit document=D module=m"),
												result),
					() -> assertTrue(! push.containsKey("bizId") && ! push.containsKey("load"), "the push has only the context: " + push));
	}

	@Test
	void aStepInAWindowTheScriptClosedOrDidNotOpenIsUnmappedOncePerSwitch() {
		List<String> result = ops(List.of(
				step("op", "navigateLink", "text", "Open", "window", "new", "url", LINK_URL, "windowId", "w0"),
				step("op", "navigateEdit", "module", "m", "document", "D", "windowId", "w1", "opened", "true", "url", LINK_URL),
				step("op", "save", "windowId", "w0"),
				step("op", "save", "windowId", "w1"),
				step("op", "ok", "windowId", "w1"),
				step("op", "save", "windowId", "w9")));
		assertAll(() -> assertEquals(9, result.size(), result.toString()),
					() -> assertEquals("closeWindow", result.get(2)),
					() -> assertTrue(result.get(4).startsWith("unmapped text=the steps below were made in a window that the script has closed"), result.toString()),
					() -> assertEquals("ok", result.get(6)),
					() -> assertTrue(result.get(7).startsWith("unmapped text=the steps below were made in another browser window"), result.toString()));
	}

	@Test
	void entriesWithoutAWindowAndOtherContextAreKept() {
		List<Map<String, String>> entries = List.of(
				step("op", "navigateMenu", "module", "m", "item", "Things"),
				step("op", "context", "text", "shown: info growl \"Saved\"", "windowId", "w0"),
				step("op", "context", "focus", "true", "windowId", "w0"));
		assertEquals(List.of("navigateMenu module=m", "context text=shown: info growl \"Saved\""), ops(entries));
	}

	/**
	 * Windows send their entries independently: a new window's first page can reach the server before the link that
	 * opened it, when the link's entry waits behind an earlier request. The link, clicked first, is taken where the
	 * page is, and the window's steps stay in it.
	 */
	@Test
	void aLinkThatArrivesAfterItsWindowsFirstPageIsStillItsLink() {
		List<String> result = ops(List.of(
				step("op", "navigateEdit", "module", "kitchensink", "document", "ListGridFixture", "windowId", "w0"),
				step("op", "navigateEdit", "module", "kitchensink", "document", "LookupDescription", "windowId", "w1",
						"opened", "true", "url", LINK_URL, "referrer", FIXTURE_URL, "at", "200"),
				step("op", "dataEnter", "binding", "description", "value", "x", "windowId", "w1"),
				step("op", "navigateLink", "text", "Open", "window", "new", "url", LINK_URL, "from", FIXTURE_URL, "at", "100", "windowId", "w0"),
				step("op", "save", "windowId", "w1"),
				step("op", "ok", "windowId", "w0")));
		assertEquals(List.of("navigateEdit document=ListGridFixture module=kitchensink",
								"navigateLink text=Open",
								"pushEditContext document=LookupDescription module=kitchensink",
								"dataEnter",
								"save",
								"closeWindow",
								"ok"),
						result);
	}

	/** A later link clicked after the page loaded is not the page's link: the page's window was opened some other way. */
	@Test
	void aLinkClickedAfterTheWindowsFirstPageIsNotItsLink() {
		List<String> result = ops(List.of(
				step("op", "navigateEdit", "module", "kitchensink", "document", "ListGridFixture", "windowId", "w0"),
				step("op", "navigateEdit", "module", "kitchensink", "document", "LookupDescription", "windowId", "w1",
						"opened", "true", "url", LINK_URL, "pageHref", "?a=e&m=kitchensink&d=LookupDescription", "at", "100"),
				step("op", "navigateLink", "text", "Open", "window", "new", "url", LINK_URL, "at", "200", "windowId", "w0")));
		assertEquals(List.of("navigateEdit document=ListGridFixture module=kitchensink",
								"navigateLink href=?a=e&m=kitchensink&d=LookupDescription",
								"pushEditContext document=LookupDescription module=kitchensink",
								"closeWindow",
								"navigateLink text=Open"),
						result);
	}

	/**
	 * A link whose window lands on another URL (a redirect, or a link that opens another URL than its own) is still
	 * that window's link: the page's referrer is the page the link was on. A window with no referrer (a typed URL) is
	 * not.
	 */
	@Test
	void aLinksWindowIsMatchedByItsReferrerAfterARedirect() {
		List<String> redirected = ops(List.of(
				step("op", "navigateLink", "text", "Open", "window", "new", "url", LINK_URL, "from", FIXTURE_URL, "windowId", "w0"),
				step("op", "navigateEdit", "module", "admin", "document", "ChangePassword", "windowId", "w1", "opened", "true",
						"url", "http://host/skyve/?a=e&m=admin&d=ChangePassword", "referrer", FIXTURE_URL),
				step("op", "save", "windowId", "w1")));
		List<String> typed = ops(List.of(
				step("op", "navigateLink", "text", "PDF", "window", "new", "url", "http://host/skyve/resources?_n=a.pdf", "from", FIXTURE_URL, "windowId", "w0"),
				step("op", "navigateEdit", "module", "admin", "document", "Contact", "windowId", "w1", "opened", "true",
						"url", "http://host/skyve/?a=e&m=admin&d=Contact", "pageHref", "?a=e&m=admin&d=Contact", "referrer", "")));
		assertAll(() -> assertEquals(List.of("navigateLink text=Open", "pushEditContext document=ChangePassword module=admin", "save"), redirected),
					() -> assertEquals(List.of("navigateLink text=PDF", "navigateLink href=?a=e&m=admin&d=Contact",
												"pushEditContext document=Contact module=admin"), typed));
	}

	/** A link's window that showed another page first (a PDF that led to a Skyve page) opens its Skyve page by its URL. */
	@Test
	void aLinksWindowThatShowedAnotherPageFirstOpensItsPageByItsUrl() {
		List<String> result = ops(List.of(
				step("op", "navigateLink", "text", "PDF", "window", "new", "url", "http://host/skyve/resources?_n=a.pdf", "from", FIXTURE_URL, "windowId", "w0"),
				step("op", "navigateEdit", "module", "admin", "document", "Contact", "windowId", "w1", "opened", "true",
						"url", "http://host/skyve/?a=e&m=admin&d=Contact", "referrer", FIXTURE_URL, "windowPages", "2")));
		assertEquals(List.of("navigateLink text=PDF", "navigateEdit document=Contact module=admin"), result);
	}

	/**
	 * A window opened among the steps that went back to the Control Panel to stop (a link on the Control Panel) is not
	 * in the script, so neither is its closeWindow; a window the script opened still closes.
	 */
	@Test
	void aWindowOpenedWhileStoppingIsNotClosedInTheScript() {
		String xml = SailRecordingBuilder.toXml(new SailRecordingBuilder(null, "admin", 0L, 1L).build(List.of(
				step("op", "navigateEdit", "module", "kitchensink", "document", "ListGridFixture", "windowId", "w0"),
				step("op", "navigateMenu", "module", "admin", "item", "DevOps/Control Panel", "target", "admin.ControlPanel", "windowId", "w0"),
				step("op", "navigateLink", "text", "Help", "window", "new", "url", "http://host/help", "view", "admin.ControlPanel", "windowId", "w0"),
				step("op", "context", "focus", "true", "windowId", "w0"))));
		assertAll(() -> assertTrue(! xml.contains("<closeWindow/>"), xml),
					() -> assertTrue(! xml.contains("Help"), xml));
	}

	/** The builder writes the steps the pass makes, and closes the windows when the user went back to stop. */
	@Test
	void theBuilderWritesTheWindowSteps() {
		SailRecordingBuilder.Result result = new SailRecordingBuilder(null, "admin", 0L, 1L).build(List.of(
				step("op", "navigateLink", "text", "Open", "window", "new", "url", LINK_URL, "windowId", "w0"),
				step("op", "navigateEdit", "module", "kitchensink", "document", "LookupDescription", "windowId", "w1",
						"opened", "true", "url", LINK_URL),
				step("op", "navigateEdit", "module", "kitchensink", "document", "LookupDescription", "windowId", "w1", "load", "reload"),
				step("op", "listGridTool", "module", "kitchensink", "query", "qListGridFixtureVue", "tool", "clearSelection", "windowId", "w1"),
				step("op", "navigateMenu", "module", "admin", "item", "DevOps/Control Panel", "target", "admin.ControlPanel", "windowId", "w0")));
		String xml = SailRecordingBuilder.toXml(result);
		assertAll(() -> assertTrue(xml.contains("<navigateLink text=\"Open\" window=\"new\"/>"), xml),
					() -> assertTrue(xml.contains("<pushEditContext module=\"kitchensink\" document=\"LookupDescription\"/>"), xml),
					() -> assertTrue(xml.contains("<refresh/>"), xml),
					() -> assertTrue(xml.contains("<listGridTool module=\"kitchensink\" query=\"qListGridFixtureVue\" tool=\"clearSelection\"/>"), xml),
					() -> assertTrue(xml.contains("<closeWindow/>"), "the window closed to stop in the first window: " + xml),
					() -> assertTrue(! xml.contains("DevOps/Control Panel"), xml),
					() -> assertTrue(! xml.contains("windowId") && ! xml.contains("opened") && ! xml.contains("url="), xml));
	}
}
