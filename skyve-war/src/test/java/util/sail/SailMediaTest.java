package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.openqa.selenium.json.Json;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.web.UserAgentType;

import util.sail.SailMedia.Frame;
import util.sail.SailMedia.RunInfo;
import util.sail.SailMedia.StepRecord;

/**
 * Tests the pages that SAIL step snapshots and video write: the film strip, the player and its data, and the list
 * of runs. No browser is needed.
 */
@SuppressWarnings({"static-method", "java:S5960"}) // test-only assertions
class SailMediaTest {
	private static final RunInfo INFO = new RunInfo("20261106-120000-000-KitchenSinkIT.test", "KitchenSinkIT.test", "01-setup.xml",
													"2026-11-06 12:00:00", "tablet");
	private static final List<StepRecord> STEPS = List.of(
			new StepRecord(1, "Contacts: create", 1, "<navigateMenu module=\"admin\" item=\"Contacts\"/>", 1000L, 1250L, null, "001-Contacts-create-navigateMenu.png", null),
			new StepRecord(2, "Contacts: create", 2, "<dataEnter binding=\"name\" value=\"a &lt; b\"/>", 1250L, 1300L,
							"Interaction \"Contacts: create\" step 2: <boom> & more", "002-Contacts-create-dataEnter.png", "002-Contacts-create-dataEnter.html"),
			new StepRecord(3, "Contacts: create", 3, "<save/>", 1300L, 1700L, "second failure", null, null));

	@Test
	void theFilmStripHasARowPerStepAndLeadsToTheFirstFailure() {
		String html = SailMedia.filmStripHtml(INFO, STEPS, "../video/player.html");
		assertAll(() -> assertEquals(3, html.split("<tr id=\"step-", -1).length - 1, "one row per step"),
					() -> assertEquals(2, html.split("<tr id=\"step-[0-9]+\" class=\"fail\"", -1).length - 1, "the failed steps are highlighted"),
					() -> assertTrue(html.contains("2 failed</span> &middot; <a href=\"#step-2\">jump to the first failure</a>"), "the first failure link"),
					() -> assertTrue(html.contains("&lt;dataEnter binding=&quot;name&quot; value=&quot;a &amp;lt; b&quot;/&gt;"), "the step's XML, escaped"),
					() -> assertTrue(html.contains("&lt;boom&gt; &amp; more"), "the failure, escaped"),
					() -> assertTrue(html.contains("<img loading=\"lazy\" src=\"001-Contacts-create-navigateMenu.png\""), "the screenshot"),
					() -> assertTrue(html.contains("<a href=\"002-Contacts-create-dataEnter.html\">page source</a>"), "the page source"),
					() -> assertTrue(html.contains("<td class=\"num\">250 ms</td>"), "the duration"),
					() -> assertTrue(html.contains("<a href=\"../video/player.html\">video</a>"), "the video"),
					() -> assertTrue(html.contains("KitchenSinkIT.test &middot; 01-setup.xml"), "the test and the script"));
	}

	@Test
	void aFilmStripWithoutFailuresSaysSo() {
		String html = SailMedia.filmStripHtml(INFO, STEPS.subList(0, 1), null);
		assertAll(() -> assertTrue(html.contains("no failures")),
					() -> assertFalse(html.contains("jump to the first failure")),
					() -> assertFalse(html.contains(">video<")));
	}

	@Test
	@SuppressWarnings("unchecked")
	void theVideoDataHasTheFramesAndTheStepsWithTheirTimes() {
		String script = SailMedia.videoScript(INFO, List.of(new Frame("frames/000001.jpg", 900L), new Frame("frames/000002.jpg", 1260L)),
												STEPS, "../snapshots/index.html");
		assertTrue(script.startsWith("window.SAIL_VIDEO = ") && script.endsWith(";\n"), script);
		Map<String, Object> video = new Json().toType(script.substring("window.SAIL_VIDEO = ".length(), script.length() - 2), Json.MAP_TYPE);
		List<List<Object>> frames = (List<List<Object>>) video.get("frames");
		List<Map<String, Object>> steps = (List<Map<String, Object>>) video.get("steps");
		assertAll(() -> assertEquals("../snapshots/index.html", video.get("filmStrip")),
					() -> assertEquals(List.of("frames/000002.jpg", Long.valueOf(1260L)), frames.get(1)),
					() -> assertEquals(3, steps.size()),
					() -> assertEquals(Long.valueOf(1250L), steps.get(1).get("s")),
					() -> assertEquals(Long.valueOf(1300L), steps.get(1).get("e")),
					() -> assertEquals("<dataEnter binding=\"name\" value=\"a &lt; b\"/>", steps.get(1).get("x")),
					() -> assertFalse(steps.get(0).containsKey("f"), "a passed step has no failure"),
					() -> assertEquals("second failure", steps.get(2).get("f")));
	}

	@Test
	void thePlayerPlaysTheVideoDataWithItsControls() {
		String html = SailMedia.playerHtml(INFO);
		assertAll(() -> assertTrue(html.contains("<script src=\"video.js\"></script>"), "loads its data"),
					() -> assertTrue(html.contains("<button id=\"play\" type=\"button\">Play</button>"), "play and pause"),
					() -> assertTrue(html.contains("<option value=\"0.5\">0.5x</option>") && html.contains("<option value=\"4\">4x</option>"), "speed"),
					() -> assertTrue(html.contains("<ol id=\"steps\"></ol>"), "the step list"),
					() -> assertTrue(html.contains("F[i+1][1]-F[i][1]"), "frames at their real timing"),
					() -> assertTrue(html.contains("V.filmStrip"), "the film strip link"));
	}

	@Test
	void theListOfRunsShowsEachRunNewestFirst(@TempDir Path base) throws IOException {
		Path older = Files.createDirectories(base.resolve("20261106-110000-000-A.test"));
		Files.writeString(older.resolve("index.html"), "strip");
		Path newer = Files.createDirectories(base.resolve("20261106-120000-000-B.test"));
		Files.writeString(newer.resolve("player.html"), "player");
		Files.createDirectories(base.resolve("not-a-run"));
		String html = SailMedia.runsIndexHtml(base);
		assertAll(() -> assertTrue(html.contains("2 runs, newest first")),
					() -> assertTrue(html.indexOf("B.test") < html.indexOf("A.test"), "newest first"),
					() -> assertTrue(html.contains("<a href=\"20261106-110000-000-A.test/index.html\">steps</a>")),
					() -> assertTrue(html.contains("<a href=\"20261106-120000-000-B.test/player.html\">play</a>")),
					() -> assertFalse(html.contains("not-a-run")));
	}

	@Test
	void aStepIsMarshalledAsOneLineOfSail() {
		Automation automation = new Automation();
		automation.setUxui("tablet");
		automation.setUserAgentType(UserAgentType.tablet);
		DataEnter step = new DataEnter();
		step.setBinding("text");
		step.setValue("a < b");
		assertEquals("<dataEnter binding=\"text\" value=\"a &lt; b\"/>", SailMedia.marshal(automation, step));
	}

	@Test
	void passwordsAreHidden() {
		assertAll(() -> assertEquals("<login password=\"********\" user=\"admin\"/>", SailMedia.hidePasswords("<login password=\"admin\" user=\"admin\"/>")),
					() -> assertEquals("<login password=\"{var:password}\" user=\"a\"/>", SailMedia.hidePasswords("<login password=\"{var:password}\" user=\"a\"/>")),
					() -> assertEquals("<save/>", SailMedia.hidePasswords("<save/>")));
	}

	@Test
	void namesAreSafeForFiles() {
		assertAll(() -> assertEquals("Contacts-create-save", SailMedia.sanitise("Contacts: create / save", 80)),
					() -> assertEquals("abc", SailMedia.sanitise("abc-def", 4)),
					() -> assertEquals("step", SailMedia.sanitise("::", 10)),
					() -> assertEquals("dataEnter", SailMedia.opCode("<dataEnter binding=\"x\"/>")),
					() -> assertEquals("step", SailMedia.opCode("")));
	}
}
