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
