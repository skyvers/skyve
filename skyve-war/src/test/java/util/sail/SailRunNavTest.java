package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.openqa.selenium.json.Json;

import util.sail.SailMedia.Place;
import util.sail.SailMedia.RunInfo;
import util.sail.SailMedia.StepRecord;
import util.sail.SailMedia.Story;

/**
 * Tests the run pages' previous and next run buttons: the directory's runs in the list's order
 * ({@value SailMedia#RUNS_ORDER_JS}), and the buttons the film strip and player carry for them. The buttons' links are
 * made in the browser; SailMediaPagesSailIT follows them.
 */
@SuppressWarnings({"static-method", "java:S5960"}) // test-only assertions
class SailRunNavTest {
	private static final Story NO_STORY = new Story(null, null, null, null);
	private static final List<StepRecord> STEPS = List.of(new StepRecord(1, "Open", 1, "Save", "<save/>", 0L, 10L, null, null, null, null));

	/** The runs' order is the list's: sessions newest first, each session's runs as they ran, with their pages and outcomes. */
	@Test
	void theRunsOrderIsTheListsWithEachRunsPages(@TempDir Path base) throws IOException {
		run(base, "20261106-090000-000-A.test", "A1 older session", new Place("20261106-085900-000", 0, 0), true, true, true);
		Place newer = new Place("20261106-115900-000", 0, 0);
		run(base, "20261106-120000-000-B.test", "B1 steps only", newer, true, false, true);
		run(base, "20261106-120100-000-B.test", "B2 video only", newer, false, true, false);
		String script = SailRunsList.runsOrderScript(SailRunsList.listedRuns(base));
		assertTrue(script.startsWith("window.SAIL_RUNS = ") && script.endsWith(";\n"), script);
		List<Map<String, Object>> runs = new Json().toType(script.substring("window.SAIL_RUNS = ".length(), script.length() - 2), Json.LIST_OF_MAPS_TYPE);
		assertAll(() -> assertEquals(List.of("20261106-120000-000-B.test", "20261106-120100-000-B.test", "20261106-090000-000-A.test"),
										runs.stream().map(r -> r.get("n")).toList(), "the list's order: the newer session first"),
					() -> assertEquals("B1 steps only", runs.get(0).get("t")),
					() -> assertEquals(Boolean.TRUE, runs.get(0).get("s"), "a film strip"),
					() -> assertEquals(Boolean.FALSE, runs.get(0).get("v"), "no player"),
					() -> assertEquals(Boolean.FALSE, runs.get(1).get("s")),
					() -> assertEquals(Boolean.TRUE, runs.get(1).get("v")),
					() -> assertEquals(Boolean.TRUE, runs.get(1).get("f"), "failed"),
					() -> assertEquals(Boolean.FALSE, runs.get(0).get("f")));
	}

	/** The film strip and player carry the buttons, hidden until the runs' order fills them in, and load the order. */
	@Test
	void theRunPagesCarryTheButtonsAndLoadTheOrder() {
		RunInfo info = new RunInfo("20261106-120000-000-B.test", "B1", "B.test", null, "2026-11-06 12:00:00", "desktop", List.of(), NO_STORY);
		String strip = SailRunPages.filmStripHtml(info, STEPS, "player.html");
		String player = SailRunPages.playerHtml(info, STEPS, "index.html");
		assertAll(() -> assertTrue(strip.contains("<nav class=\"nav\" data-run=\"20261106-120000-000-B.test\" data-page=\"steps\">"), strip),
					() -> assertTrue(player.contains("<nav class=\"nav\" data-run=\"20261106-120000-000-B.test\" data-page=\"video\">"), player),
					() -> assertTrue(strip.contains("<a id=\"prevRun\" class=\"btn\" hidden>&lsaquo; Run</a><a class=\"btn\" href=\"../index.html\">All runs</a>" +
													"<a id=\"nextRun\" class=\"btn\" hidden>Run &rsaquo;</a>"), "beside All runs, hidden"),
					() -> assertTrue(strip.contains("<span id=\"runPos\"></span>"), "the place in the list"),
					() -> assertTrue(strip.contains("<script src=\"../runs-order.js\"></script>"), "loads the order"),
					() -> assertTrue(player.contains("<script src=\"../runs-order.js\"></script>")));
	}

	/** Ending a run writes the directory's runs' order beside its list. */
	@Test
	void endingARunWritesTheOrderBesideTheList(@TempDir Path base) throws IOException {
		SailSettings.Environment environment = new SailSettings.Environment(Map.of(), Map.of(), null, base);
		SailSettings settings = SailSettings.resolve(environment, List.of(SailSettings.code().snapshotDir(Path.of("media")).layer()));
		org.skyve.metadata.sail.language.Automation automation = new org.skyve.metadata.sail.language.Automation();
		automation.setUxui("tablet");
		automation.setUserAgentType(org.skyve.web.UserAgentType.tablet);
		SailMedia media = SailMedia.start(org.mockito.Mockito.mock(PrimeFacesSelenide.class), automation, settings);
		assertTrue(media != null);
		media.finish(true);
		Path order = base.resolve("media").resolve(SailMedia.RUNS_ORDER_JS);
		String script = Files.readString(order);
		List<Map<String, Object>> runs = new Json().toType(script.substring("window.SAIL_RUNS = ".length(), script.length() - 2), Json.LIST_OF_MAPS_TYPE);
		assertAll(() -> assertTrue(Files.exists(base.resolve("media").resolve("index.html"))),
					() -> assertEquals(1, runs.size(), script),
					() -> assertEquals(Boolean.TRUE, runs.get(0).get("s"), "its film strip"));
	}

	/** Writes a run folder with the pages asked for and a run.json. */
	static void run(Path base, String folder, String title, Place place, boolean steps, boolean video, boolean passed) throws IOException {
		Path run = Files.createDirectories(base.resolve(folder));
		RunInfo info = new RunInfo(folder, title, "B.test", null, "2026-11-06 12:00:00", "desktop", List.of(), NO_STORY);
		if (steps) {
			Files.writeString(run.resolve("index.html"), SailRunPages.filmStripHtml(info, STEPS, video ? "player.html" : null));
		}
		if (video) {
			Files.writeString(run.resolve("player.html"), SailRunPages.playerHtml(info, STEPS, steps ? "index.html" : null));
			Files.writeString(run.resolve("video.js"), SailRunPages.videoScript(info, List.of(), STEPS, steps ? "index.html" : null));
		}
		Files.writeString(run.resolve(SailMedia.RUN_JSON), SailMedia.runJson(info, place, STEPS, passed));
	}
}
