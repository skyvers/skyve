package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
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

import util.sail.SailMedia.Place;
import util.sail.SailMedia.RunInfo;
import util.sail.SailMedia.StepRecord;
import util.sail.SailMedia.Story;

/**
 * Tests how the SAIL media pages show the descriptions of a run's suite, workflow, automation and interactions: the
 * film strip, the player and its data, the run's summary and the list of runs. No browser is needed.
 */
@SuppressWarnings({"static-method", "java:S5960"}) // test-only assertions
class SailMediaDescriptionTest {
	private static final Story NO_STORY = new Story(null, null, null, null);
	private static final Story STORY = new Story("app.suite.xml", "The admin <module>.\n\nEnd to end.",
													"Ben claims, Rob approves.", "Ben lodges the claim.");
	private static final RunInfo INFO = new RunInfo("20261120-120000-000-AppsIT.test", "Lodge", "AppsIT.test", "w2.xml",
													"2026-11-20 12:00:00", "external", List.of(), STORY);
	private static final List<StepRecord> STEPS = List.of(
			new StepRecord(1, "Lodge", 1, "Sign in", "<login/>", 0L, 10L, null, null, null, "Signs in & submits."),
			new StepRecord(2, "Lodge", 2, "Save", "<save/>", 10L, 20L, null, null, null, "Signs in & submits."),
			new StepRecord(3, "Lodge (after)", 1, "Sign out", "<logout/>", 20L, 30L, null, null, null, null),
			new StepRecord(4, "Check", 1, "Save", "<save/>", 30L, 40L, null, null, null, "Rob looks."));

	@Test
	void theFilmStripTellsTheStoryAboveTheStepsAndEachInteractionsUnderItsName() {
		String html = SailRunPages.filmStripHtml(INFO, STEPS, null);
		assertAll(() -> assertTrue(html.contains("<main class=\"content\"><section class=\"story card\">"), "the story, above the steps"),
					() -> assertTrue(html.contains("<h2>Suite <span class=\"muted\">app.suite.xml</span></h2>" +
													"<div class=\"desc\"><p>The admin &lt;module&gt;.</p><p>End to end.</p></div>"), "the suite's, escaped, by paragraph"),
					() -> assertTrue(html.contains("<h2>Workflow</h2><div class=\"desc\"><p>Ben claims, Rob approves.</p></div>"), "the workflow's"),
					() -> assertTrue(html.contains("<h2>This run</h2><div class=\"desc\"><p>Ben lodges the claim.</p></div>"), "the automation's"),
					() -> assertTrue(html.contains("<td colspan=\"5\">Lodge<div class=\"desc\"><p>Signs in &amp; submits.</p></div></td>"), "the interaction's"),
					() -> assertTrue(html.contains("<td colspan=\"5\">Lodge (after)</td>"), "an after procedure has none"),
					() -> assertTrue(html.contains("<td colspan=\"5\">Check<div class=\"desc\"><p>Rob looks.</p></div></td>")));
	}

	@Test
	void onlyThePartsWithADescriptionAreShown() {
		RunInfo info = new RunInfo("r", "T", "T.t", null, "2026-11-20 12:00:00", "desktop", List.of(),
									new Story("suite.xml", null, null, "Just this."));
		String html = SailRunPages.filmStripHtml(info, STEPS, null);
		assertAll(() -> assertTrue(html.contains("<section class=\"story card\"><div class=\"story-part\"><h2>This run</h2>"), html),
					() -> assertFalse(html.contains("<h2>Suite"), "a suite with no description"),
					() -> assertFalse(html.contains("<h2>Workflow</h2>")));
	}

	@Test
	void aRunWithNoDescriptionsHasNoStory() {
		RunInfo info = new RunInfo("r", "T", "T.t", null, "2026-11-20 12:00:00", "desktop", List.of(), NO_STORY);
		String strip = SailRunPages.filmStripHtml(info, STEPS, null);
		String player = SailRunPages.playerHtml(info, STEPS, null);
		assertAll(() -> assertFalse(strip.contains("class=\"story"), strip),
					() -> assertFalse(player.contains("<details class=\"story-bar\""), player));
	}

	@Test
	void thePlayerFoldsTheStoryAndHeadsEachGroupWithItsDescriptionOnce() {
		String player = SailRunPages.playerHtml(INFO, STEPS, null);
		String script = SailRunPages.videoScript(INFO, List.of(), STEPS, null);
		Map<String, Object> video = new Json().toType(script.substring("window.SAIL_VIDEO = ".length(), script.length() - 2), Json.MAP_TYPE);
		@SuppressWarnings("unchecked")
		List<Map<String, Object>> steps = (List<Map<String, Object>>) video.get("steps");
		assertAll(() -> assertTrue(player.contains("<details class=\"story-bar\"><summary>What this run tests</summary><div class=\"story\">"), player),
					() -> assertEquals("Signs in & submits.", steps.get(0).get("pd"), "on the group's first step"),
					() -> assertNull(steps.get(1).get("pd"), "once"),
					() -> assertNull(steps.get(2).get("pd"), "an after procedure has none"),
					() -> assertEquals("Rob looks.", steps.get(3).get("pd")));
	}

	@Test
	void theRunsSummaryHasTheStory() {
		Map<String, Object> run = new Json().toType(SailMedia.runJson(INFO, new Place("s", 0, 0), STEPS, true), Json.MAP_TYPE);
		assertAll(() -> assertEquals("app.suite.xml", run.get("suite")),
					() -> assertEquals("The admin <module>.\n\nEnd to end.", run.get("suiteDescription")),
					() -> assertEquals("Ben claims, Rob approves.", run.get("workflowDescription")),
					() -> assertEquals("Ben lodges the claim.", run.get("description")));
	}

	/** A run's story is the suite and the unit running on its thread, and its automation's description. */
	@Test
	void aRunsStoryComesFromItsSuiteItsUnitAndItsAutomation() {
		Automation automation = new Automation();
		automation.setDescription("The automation.");
		Story outside = SailMedia.story(automation);
		Story inside;
		SailMedia.suite("suite.xml", "The suite.");
		SailMedia.beginUnit("The workflow.");
		try {
			inside = SailMedia.story(automation);
		}
		finally {
			SailMedia.endUnit(false);
			SailMedia.suite(null, null);
		}
		Story after = SailMedia.story(new Automation());
		assertAll(() -> assertEquals(new Story(null, null, null, "The automation."), outside),
					() -> assertEquals(new Story("suite.xml", "The suite.", "The workflow.", "The automation."), inside),
					() -> assertEquals(NO_STORY, after, "cleared"));
	}

	/**
	 * The list of runs heads a suite's runs with the suite and its description, once; a workflow's heading has its
	 * description; a run has its own, and a workflow of one (no heading) its workflow's too.
	 */
	@Test
	void theListOfRunsTellsTheStory(@TempDir Path base) throws IOException {
		listedRun(base, "20261120-120000-000-A.test-setup.xml", new Place("20261120-110000-000", 1, 1),
					new Story("admin.suite.xml", "The admin suite.", null, "Backs up."), "setup.xml");
		listedRun(base, "20261120-120100-000-A.test-w2.xml", new Place("20261120-110000-000", 2, 1),
					new Story("admin.suite.xml", "The admin suite.", "Ben claims, Rob approves.", "Ben lodges."), "w2.xml");
		listedRun(base, "20261120-120200-000-A.test-w2.xml", new Place("20261120-110000-000", 2, 2),
					new Story("admin.suite.xml", "The admin suite.", "Ben claims, Rob approves.", "Rob approves."), "w2.xml");
		listedRun(base, "20261120-120300-000-A.test-one.xml", new Place("20261120-110000-000", 3, 1),
					new Story("admin.suite.xml", "The admin suite.", "A workflow of one.", null), "one.xml");
		listedRun(base, "20261120-120400-000-B.test", new Place("20261120-110000-000", 0, 0), NO_STORY, null);
		String html = SailRunsList.runsIndexHtml(base);
		assertAll(() -> assertEquals(1, html.split("<tr class=\"group suite\"", -1).length - 1, "one suite heading"),
					() -> assertTrue(html.contains("<tr class=\"group suite\" data-header=\"g1\"><td colspan=\"7\">Suite admin.suite.xml" +
													"<div class=\"desc\"><p>The admin suite.</p></div></td></tr>"), html),
					() -> assertTrue(html.contains("<tr class=\"group\" data-header=\"g2\"><td colspan=\"7\">setup.xml</td></tr>"), "the member file under it"),
					() -> assertTrue(html.contains("<tr data-run data-failed=\"false\" data-s=\"g1\" data-g=\"g2\">"), "a run names its suite's group"),
					() -> assertTrue(html.contains("workflow of 2 legs</div><div class=\"desc\"><p>Ben claims, Rob approves.</p></div>"), "the workflow's, on its heading"),
					() -> assertEquals(1, html.split("Ben claims, Rob approves", -1).length - 1, "not repeated on its legs"),
					() -> assertTrue(html.contains("<div class=\"desc\"><p>Rob approves.</p></div>"), "a leg's own"),
					() -> assertTrue(html.contains("<div class=\"desc\"><p>A workflow of one.</p></div>"), "a workflow of one's, on its run"),
					() -> assertTrue(html.contains("<tr data-run data-failed=\"false\"><td"), "a run outside a suite has no suite group"));
	}

	private static void listedRun(Path base, String folder, Place place, Story story, String label) throws IOException {
		Path run = Files.createDirectories(base.resolve(folder));
		Files.writeString(run.resolve("player.html"), "player");
		RunInfo info = new RunInfo(folder, "Title " + folder.substring(9, 15), "A.test", label, "2026-11-20 12:00:00", "external", List.of(), story);
		Files.writeString(run.resolve(SailMedia.RUN_JSON), SailMedia.runJson(info, place, STEPS.subList(0, 1), true));
	}
}
