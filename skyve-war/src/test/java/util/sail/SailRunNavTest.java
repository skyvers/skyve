package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.openqa.selenium.json.Json;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.web.UserAgentType;

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

	/**
	 * The runs' order is the list's: sessions newest first, each session's runs as they ran, with each run's session,
	 * pages and outcome.
	 */
	@Test
	void theRunsOrderIsTheListsWithEachRunsPages(@TempDir Path base) throws IOException {
		run(base, "20261106-090000-000-A.test", "A1 older session", new Place("20261106-085900-000", 0, 0), true, true, true);
		Place newer = new Place("20261106-115900-000", 0, 0);
		run(base, "20261106-120000-000-B.test", "B1 steps only", newer, true, false, true);
		run(base, "20261106-120100-000-B.test", "B2 video only", newer, false, true, false);
		List<Map<String, Object>> runs = order(SailRunsList.runsOrderScript(SailRunsList.listedRuns(base)));
		assertAll(() -> assertEquals(List.of("20261106-120000-000-B.test", "20261106-120100-000-B.test", "20261106-090000-000-A.test"),
										runs.stream().map(r -> r.get("n")).toList(), "the list's order: the newer session first"),
					() -> assertEquals(List.of("20261106-115900-000", "20261106-115900-000", "20261106-085900-000"),
										runs.stream().map(r -> r.get("e")).toList(), "each run's session"),
					() -> assertEquals("B1 steps only", runs.get(0).get("t")),
					() -> assertEquals(Boolean.TRUE, runs.get(0).get("s"), "a film strip"),
					() -> assertEquals(Boolean.FALSE, runs.get(0).get("v"), "no player"),
					() -> assertEquals(Boolean.FALSE, runs.get(1).get("s")),
					() -> assertEquals(Boolean.TRUE, runs.get(1).get("v")),
					() -> assertEquals(Boolean.TRUE, runs.get(1).get("f"), "failed"),
					() -> assertEquals(Boolean.FALSE, runs.get(0).get("f")));
	}

	/** A title with quotes, a script end tag and other scripts' letters stays data in the order, and in the page. */
	@Test
	void aRunsTitleAndNameStayData(@TempDir Path base) throws IOException {
		String title = "Ä \"quoted\" </script><b>bold</b> 100% \\ 日本";
		run(base, "20261106-120000-000-B.test", title, new Place("20261106-115900-000", 0, 0), true, false, true);
		String script = SailRunsList.runsOrderScript(SailRunsList.listedRuns(base));
		String strip = SailRunPages.filmStripHtml(new RunInfo("a\"b<c", title, "B.test", null, "2026-11-06 12:00:00", "desktop", List.of(), NO_STORY),
													STEPS, null);
		assertAll(() -> assertEquals(title, order(script).get(0).get("t"), "the title, as it was"),
					() -> assertTrue(strip.contains("data-run=\"a&quot;b&lt;c\""), "the run's name, escaped in the page"));
	}

	/** The film strip and player carry the buttons and their place, hidden until the runs' order fills them in, and load it. */
	@Test
	void theRunPagesCarryTheButtonsAndLoadTheOrder() {
		RunInfo info = new RunInfo("20261106-120000-000-B.test", "B1", "B.test", null, "2026-11-06 12:00:00", "desktop", List.of(), NO_STORY);
		String strip = SailRunPages.filmStripHtml(info, STEPS, "player.html");
		String player = SailRunPages.playerHtml(info, STEPS, "index.html");
		assertAll(() -> assertTrue(strip.contains("<nav class=\"nav\" data-run=\"20261106-120000-000-B.test\" data-page=\"steps\">"), strip),
					() -> assertTrue(player.contains("<nav class=\"nav\" data-run=\"20261106-120000-000-B.test\" data-page=\"video\">"), player),
					() -> assertTrue(strip.contains("<a id=\"prevRun\" class=\"btn\" hidden>&lsaquo; Run</a><a class=\"btn\" href=\"../index.html\">All runs</a>" +
													"<a id=\"nextRun\" class=\"btn\" hidden>Run &rsaquo;</a>"), "beside All runs, hidden"),
					() -> assertTrue(strip.contains("<span id=\"runPos\" hidden></span>"), "the place, hidden"),
					() -> assertTrue(strip.contains("<script src=\"../runs-order.js\"></script>"), "loads the order"),
					() -> assertTrue(player.contains("<script src=\"../runs-order.js\"></script>")));
	}

	/** Ending a run writes the directory's runs' order beside its list. */
	@Test
	void endingARunWritesTheOrderBesideTheList(@TempDir Path base) throws IOException {
		SailSettings settings = settings(base, SailSettings.code().snapshotDir(Path.of("media")));
		endRun(settings, "one", true);
		List<Map<String, Object>> runs = order(Files.readString(base.resolve("media").resolve(SailMedia.RUNS_ORDER_JS)));
		assertAll(() -> assertTrue(Files.exists(base.resolve("media").resolve("index.html"))),
					() -> assertEquals(1, runs.size()),
					() -> assertEquals(Boolean.TRUE, runs.get(0).get("s"), "its film strip"));
	}

	/** With keepMedia=failed, a passed run's folder is deleted and it leaves the order; the failed run stays. */
	@Test
	void aDeletedRunLeavesTheOrder(@TempDir Path base) throws IOException {
		SailSettings settings = settings(base, SailSettings.code().snapshotDir(Path.of("media")).keepMedia(SailSettings.KeepMedia.FAILED));
		endRun(settings, "failed", false);
		endRun(settings, "passed", true);
		List<Map<String, Object>> runs = order(Files.readString(base.resolve("media").resolve(SailMedia.RUNS_ORDER_JS)));
		assertAll(() -> assertEquals(1, runs.size(), runs.toString()),
					() -> assertTrue(String.valueOf(runs.get(0).get("n")).endsWith("-failed"), runs.toString()));
	}

	/** Snapshots and video in different directories: each directory has its own order, with the pages it holds. */
	@Test
	void separateDirectoriesHaveTheirOwnOrders(@TempDir Path base) throws IOException {
		SailSettings settings = settings(base, SailSettings.code().snapshotDir(Path.of("snapshots")).videoDir(Path.of("video")));
		endRun(settings, "both", true);
		List<Map<String, Object>> snapshots = order(Files.readString(base.resolve("snapshots").resolve(SailMedia.RUNS_ORDER_JS)));
		List<Map<String, Object>> video = order(Files.readString(base.resolve("video").resolve(SailMedia.RUNS_ORDER_JS)));
		assertAll(() -> assertEquals(Boolean.TRUE, snapshots.get(0).get("s")),
					() -> assertEquals(Boolean.FALSE, snapshots.get(0).get("v"), "its player is in the other directory"),
					() -> assertEquals(Boolean.FALSE, video.get(0).get("s")),
					() -> assertEquals(Boolean.TRUE, video.get(0).get("v")));
	}

	private static SailSettings settings(Path base, SailSettings.Code code) {
		return SailSettings.resolve(new SailSettings.Environment(Map.of(), Map.of(), null, base), List.of(code.layer()));
	}

	/** Starts and ends a run of one step, labelled, that passed or failed. */
	private static void endRun(SailSettings settings, String label, boolean passed) {
		Automation automation = new Automation();
		automation.setUxui("tablet");
		automation.setUserAgentType(UserAgentType.tablet);
		SailMedia.label(label); // names the run's folder
		SailMedia media;
		try {
			media = SailMedia.start(mock(PrimeFacesSelenide.class), automation, settings);
		}
		finally {
			SailMedia.label(null);
		}
		assertNotNull(media);
		media.finish(passed);
	}

	private static List<Map<String, Object>> order(String script) {
		assertTrue(script.startsWith("window.SAIL_RUNS = ") && script.endsWith(";\n"), script);
		return new Json().toType(script.substring("window.SAIL_RUNS = ".length(), script.length() - 2), Json.LIST_OF_MAPS_TYPE);
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
