package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
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
import org.skyve.metadata.sail.language.Interaction;
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
	private static final RunInfo INFO = new RunInfo("20261106-120000-000-KitchenSinkIT.test", "Contacts: create", "KitchenSinkIT.test", "01-setup.xml",
													"2026-11-06 12:00:00", "tablet",
													List.of("testStrategy=Verify (-Dsail.testStrategy)", "var.password=******** (code)"));
	private static final List<StepRecord> STEPS = List.of(
			new StepRecord(1, "Contacts: create", 1, "<navigateMenu module=\"admin\" item=\"Contacts\"/>", 1000L, 1250L, null, "001-Contacts-create-navigateMenu.png", null),
			new StepRecord(2, "Contacts: create", 2, "<dataEnter binding=\"name\" value=\"a &lt; b\"/>", 1250L, 1300L,
							"Interaction \"Contacts: create\" step 2: <boom> & more", "002-Contacts-create-dataEnter.png", "002-Contacts-create-dataEnter.html"),
			new StepRecord(3, "Contacts: create", 3, "<save/>", 1300L, 1700L, "second failure", null, null));

	@Test
	void theFilmStripHasARowPerStepAndLeadsToTheFirstFailure() {
		String html = SailMedia.filmStripHtml(INFO, STEPS, "../video/player.html");
		assertAll(() -> assertEquals(3, html.split("<tr id=\"step-", -1).length - 1, "one row per step"),
					() -> assertEquals(1, html.split("<tr class=\"group\">", -1).length - 1, "one heading for the steps' interaction"),
					() -> assertEquals(2, html.split("<tr id=\"step-[0-9]+\" class=\"fail\"", -1).length - 1, "the failed steps are highlighted"),
					() -> assertTrue(html.contains("<span class=\"badge fail\">2 failed</span>"), "the outcome"),
					() -> assertTrue(html.contains("<a href=\"#step-2\">Jump to the first failure</a>"), "the first failure link"),
					() -> assertTrue(html.contains("<div class=\"summary\">Data enter (binding: name, value: a &lt; b)</div>"), "the step in words"),
					() -> assertTrue(html.contains("&lt;dataEnter binding=&quot;name&quot; value=&quot;a &amp;lt; b&quot;/&gt;"), "the step's XML, escaped"),
					() -> assertTrue(html.contains("&lt;boom&gt; &amp; more"), "the failure, escaped"),
					() -> assertTrue(html.contains("<a target=\"_blank\" rel=\"noopener\" href=\"001-Contacts-create-navigateMenu.png\">" +
													"<img loading=\"lazy\" src=\"001-Contacts-create-navigateMenu.png\""), "the screenshot, opening in a new tab"),
					() -> assertTrue(html.contains("target=\"_blank\" rel=\"noopener\" href=\"002-Contacts-create-dataEnter.html\">page source</a>"), "the page source, in a new tab"),
					() -> assertTrue(html.contains("<td class=\"num\">250 ms</td>"), "the duration"),
					() -> assertTrue(html.contains("<a class=\"btn\" href=\"../video/player.html\">Video</a>"), "the video"),
					() -> assertTrue(html.contains("<h1>Contacts: create</h1>"), "the run's title"),
					() -> assertTrue(html.contains("KitchenSinkIT.test &middot; 01-setup.xml"), "the test and the script"),
					() -> assertTrue(html.contains("popovertarget=\"settings\">Settings</button>"), "the settings are behind a button"),
					() -> assertTrue(html.contains("<dt>testStrategy</dt><dd>Verify <span class=\"muted\">(-Dsail.testStrategy)</span></dd>"),
										"the settings with their sources"),
					() -> assertTrue(html.contains("<dt>var.password</dt><dd>******** <span class=\"muted\">(code)</span></dd>"),
										"a masked setting"));
	}

	@Test
	void aFilmStripWithoutFailuresSaysSo() {
		String html = SailMedia.filmStripHtml(INFO, STEPS.subList(0, 1), null);
		assertAll(() -> assertTrue(html.contains("<span class=\"badge pass\">Passed</span>")),
					() -> assertFalse(html.contains("Jump to the first failure")),
					() -> assertFalse(html.contains(">Video<")));
	}

	@Test
	void aRunWithDefaultSettingsSaysSo() {
		RunInfo info = new RunInfo("r", "T.t", "T.t", null, "2026-11-17 12:00:00", "desktop", List.of());
		String html = SailMedia.filmStripHtml(info, STEPS, null);
		assertTrue(html.contains("Settings: all defaults"), html);
	}

	/**
	 * With keepMedia=failed, a run that passed deletes its folder and is no longer listed; a run with a failed step, or
	 * one that failed outside its steps, keeps it; with keepMedia=all, a passed run keeps it.
	 */
	@Test
	void onlyAFailedRunKeepsItsMediaWhenAskedTo(@TempDir Path base) throws IOException {
		SailSettings.Environment environment = new SailSettings.Environment(Map.of(), Map.of(), base.resolve("home"), base);
		SailSettings failedOnly = SailSettings.resolve(environment, List.of(SailSettings.code().snapshotDir(Path.of("media"))
																					.keepMedia(SailSettings.KeepMedia.FAILED).layer()));
		SailSettings all = SailSettings.resolve(environment, List.of(SailSettings.code().snapshotDir(Path.of("media")).layer()));
		Path media = base.resolve("media");

		Path passed = run(failedOnly, media, "passed", null, true);
		Path failedStep = run(failedOnly, media, "failedStep", "a failure", true);
		Path failedAutomation = run(failedOnly, media, "failedAutomation", null, false);
		Path kept = run(all, media, "kept", null, true);
		String index = Files.readString(media.resolve("index.html"));
		assertAll(() -> assertFalse(Files.exists(passed), "a passed run's folder is deleted"),
					() -> assertTrue(Files.exists(failedStep.resolve("index.html")), "a run with a failed step keeps its film strip"),
					() -> assertTrue(Files.exists(failedAutomation.resolve("index.html")), "a failed run keeps its film strip"),
					() -> assertTrue(Files.exists(kept.resolve("index.html")), "keepMedia=all keeps a passed run's film strip"),
					() -> assertTrue(index.contains("3 runs"), index));
	}

	/**
	 * In a unit (a workflow), keepMedia=failed deletes the passed runs' folders only when the unit ends having passed:
	 * a unit that failed later (a later leg) keeps them all.
	 */
	@Test
	void aUnitKeepsItsRunsMediaUnlessTheWholeUnitPassed(@TempDir Path base) {
		SailSettings.Environment environment = new SailSettings.Environment(Map.of(), Map.of(), null, base);
		SailSettings failedOnly = SailSettings.resolve(environment, List.of(SailSettings.code().snapshotDir(Path.of("media"))
																					.keepMedia(SailSettings.KeepMedia.FAILED).layer()));
		Path media = base.resolve("media");
		SailMedia.beginUnit();
		Path first;
		Path second;
		try {
			first = run(failedOnly, media, "unitPassed1", null, true);
			second = run(failedOnly, media, "unitPassed2", null, true);
			assertAll(() -> assertTrue(Files.exists(first), "kept until the unit ends"),
						() -> assertTrue(Files.exists(second), "kept until the unit ends"));
		}
		finally {
			SailMedia.endUnit(true);
		}
		SailMedia.beginUnit();
		Path kept;
		try {
			kept = run(failedOnly, media, "unitFailedLater", null, true);
		}
		finally {
			SailMedia.endUnit(false);
		}
		assertAll(() -> assertFalse(Files.exists(first), "the passed unit's runs are deleted"),
					() -> assertFalse(Files.exists(second), "the passed unit's runs are deleted"),
					() -> assertTrue(Files.exists(kept.resolve("index.html")), "a failed unit keeps its passed runs"));
	}

	/** Runs one step in a run's media, labelled, and ends it; returns the run's folder. */
	private static Path run(SailSettings settings, Path base, String label, String failure, boolean passed) {
		PrimeFacesSelenide selenide = mock(PrimeFacesSelenide.class);
		Automation automation = new Automation();
		automation.setUxui("tablet");
		automation.setUserAgentType(UserAgentType.tablet);
		Interaction interaction = new Interaction();
		interaction.setName("Interaction");
		automation.getInteractions().add(interaction);
		SailMedia.label(label); // names the run's folder
		SailMedia media;
		try {
			media = SailMedia.start(selenide, automation, settings);
		}
		finally {
			SailMedia.label(null);
		}
		assertNotNull(media);
		DataEnter step = new DataEnter();
		step.setBinding("text");
		step.setValue("x");
		media.stepStarted("Interaction", 1, step);
		media.stepEnded(failure);
		media.finish(passed);
		// the folder is named by the time, the test (outside the harness: none here) and the label
		String name = "-" + label;
		try (java.util.stream.Stream<Path> runs = Files.list(base)) {
			return runs.filter(run -> run.getFileName().toString().endsWith(name)).findFirst()
						.orElse(base.resolve("deleted" + name));
		}
		catch (IOException e) {
			throw new java.io.UncheckedIOException(e);
		}
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
					() -> assertEquals(INFO.settings(), video.get("settings")),
					() -> assertEquals(List.of("frames/000002.jpg", Long.valueOf(1260L)), frames.get(1)),
					() -> assertEquals(3, steps.size()),
					() -> assertEquals(Long.valueOf(1250L), steps.get(1).get("s")),
					() -> assertEquals(Long.valueOf(1300L), steps.get(1).get("e")),
					() -> assertEquals("<dataEnter binding=\"name\" value=\"a &lt; b\"/>", steps.get(1).get("x")),
					() -> assertEquals("Data enter (binding: name, value: a < b)", steps.get(1).get("d"), "the step in words"),
					() -> assertEquals("Contacts: create", video.get("title")),
					() -> assertFalse(steps.get(0).containsKey("f"), "a passed step has no failure"),
					() -> assertEquals("second failure", steps.get(2).get("f")));
	}

	@Test
	void thePlayerPlaysTheVideoDataWithItsControls() {
		String html = SailMedia.playerHtml(INFO, STEPS, "../snapshots/index.html");
		assertAll(() -> assertTrue(html.contains("<script src=\"video.js\"></script>"), "loads its data"),
					() -> assertTrue(html.contains("<button id=\"play\" class=\"btn primary\" type=\"button\">Play</button>"), "play and pause"),
					() -> assertTrue(html.contains("<button id=\"prev\"") && html.contains("<button id=\"next\""), "step by step"),
					() -> assertTrue(html.contains("<option value=\"0.5\">0.5x</option>") && html.contains("<option value=\"8\">8x</option>"), "speed"),
					() -> assertTrue(html.contains("<div id=\"steps\"></div>"), "the step list"),
					() -> assertTrue(html.contains("s.d || s.x"), "the steps in words"),
					() -> assertTrue(html.contains("F[i+1][1]-F[i][1]"), "frames at their real timing"),
					() -> assertTrue(html.contains("pin = k"), "a clicked step stays the one shown"),
					() -> assertTrue(html.contains("<a class=\"btn\" href=\"../snapshots/index.html\">Steps</a>"), "the film strip link"),
					() -> assertTrue(html.contains("<h1>Contacts: create</h1>") && html.contains("2 failed"), "the run's title and outcome"),
					() -> assertTrue(html.contains("<div id=\"settings\" class=\"pop\" popover>"), "the settings in a pop-up"));
	}

	/**
	 * The list of runs shows sessions newest first, and each session's runs in the order they ran, under their suite
	 * files, with a unit's legs under the unit; runs without a run.json come last, by their folders.
	 */
	@Test
	void theListOfRunsFollowsEachSessionsSuite(@TempDir Path base) throws IOException {
		Path old = Files.createDirectories(base.resolve("20261105-090000-000-Old.test"));
		Files.writeString(old.resolve("index.html"), "strip");
		listedRun(base, "20261106-090000-000-A.test", "A1 earlier session", null, new SailMedia.Place("20261106-085900-000", 0, 0), true);
		SailMedia.Place none = new SailMedia.Place("20261106-115900-000", 0, 0);
		listedRun(base, "20261106-120000-000-B.test", "HM1 home lists inspections", "10-home.xml", none, true);
		listedRun(base, "20261106-120100-000-B.test", "HM2 phone: works offline", "10-home.xml", new SailMedia.Place(none.session(), 7, 1), true);
		listedRun(base, "20261106-120200-000-B.test", "HM2 office: recalls it", "10-home.xml", new SailMedia.Place(none.session(), 7, 2), false);
		listedRun(base, "20261106-120300-000-B.test", "CK1 checks out", "20-checkout.xml", none, true);
		Files.createDirectories(base.resolve("not-a-run"));
		String html = SailMedia.runsIndexHtml(base);
		int newSession = html.indexOf("Session started 2026-11-06 11:59:00");
		int oldSession = html.indexOf("Session started 2026-11-06 08:59:00");
		int earlier = html.indexOf("Earlier runs");
		assertAll(() -> assertTrue(html.contains("6 runs, newest session first")),
					() -> assertTrue(html.contains("<span class=\"badge fail\">1 failed</span>"), "how many runs failed"),
					() -> assertTrue((newSession >= 0) && (newSession < oldSession) && (oldSession < earlier), "sessions newest first, unknown last"),
					() -> assertTrue(html.indexOf("HM1 home") < html.indexOf("HM2 phone") && html.indexOf("HM2 office") < html.indexOf("CK1 checks"),
										"a session's runs in the order they ran"),
					() -> assertEquals(1, html.split(">10-home.xml</td></tr>", -1).length - 1, "one heading per suite file"),
					() -> assertTrue(html.indexOf(">10-home.xml</td></tr>") < html.indexOf("HM1 home")
										&& html.indexOf(">20-checkout.xml</td></tr>") < html.indexOf("CK1 checks"), "the file's heading before its runs"),
					() -> assertTrue(html.contains("<tr class=\"unit fail\" data-header=\"g2\"><td></td><td></td><td><div class=\"summary\">HM2</div>" +
													"<div class=\"muted small\">workflow of 2 legs</div></td><td><span class=\"badge fail\">Failed</span>"),
										"a unit, titled by what its legs share, failed when a leg failed"),
					() -> assertTrue(html.contains("<tr data-run data-failed=\"false\" data-g=\"g1\" data-u=\"g2\" class=\"leg\"><td class=\"num muted\">2</td>" +
													"<td class=\"num\">12:01:00</td><td><span class=\"leg-no\">leg 1</span>"), "a leg, numbered in the session"),
					() -> assertTrue(html.contains("<span class=\"leg-no\">leg 2</span><div class=\"summary\">HM2 office: recalls it</div>"), "the second leg"),
					() -> assertTrue(html.contains("<a class=\"btn\" href=\"20261105-090000-000-Old.test/index.html\">Steps</a>")),
					() -> assertTrue(html.contains("<a class=\"btn\" href=\"20261106-120000-000-B.test/player.html\">Video</a>")),
					() -> assertTrue(html.contains("<td class=\"num\">2026-11-05 09:00:00</td><td><div class=\"summary\">20261105-090000-000-Old.test</div>"),
										"a run without a run.json by its folder"),
					() -> assertTrue(html.contains("id=\"failedOnly\"") && html.contains("id=\"find\""), "the filters"),
					() -> assertFalse(html.contains("not-a-run")));
	}

	/** Writes a run folder with a player and a run.json. */
	private static void listedRun(Path base, String folder, String title, String label, SailMedia.Place place, boolean passed) throws IOException {
		Path run = Files.createDirectories(base.resolve(folder));
		Files.writeString(run.resolve("player.html"), "player");
		RunInfo info = new RunInfo(folder, title, "B.test", label, "2026-11-06 " + folder.substring(9, 11) + ':' + folder.substring(11, 13) + ":00",
									"phone", List.of());
		Files.writeString(run.resolve(SailMedia.RUN_JSON), SailMedia.runJson(info, place, passed ? STEPS.subList(0, 1) : STEPS, passed));
	}

	/** A unit's runs record its number and their legs in order, in this session; a run outside a unit records neither. */
	@Test
	void aUnitsRunsAreItsLegsInOrder(@TempDir Path base) throws IOException {
		SailSettings.Environment environment = new SailSettings.Environment(Map.of(), Map.of(), null, base);
		SailSettings settings = SailSettings.resolve(environment, List.of(SailSettings.code().snapshotDir(Path.of("media")).layer()));
		Path media = base.resolve("media");
		Path alone = run(settings, media, "alone", null, true);
		SailMedia.beginUnit();
		Path first;
		Path second;
		try {
			first = run(settings, media, "leg1", null, true);
			second = run(settings, media, "leg2", null, true);
		}
		finally {
			SailMedia.endUnit(true);
		}
		Map<String, Object> aloneRun = runJson(alone);
		Map<String, Object> firstLeg = runJson(first);
		Map<String, Object> secondLeg = runJson(second);
		assertAll(() -> assertEquals(Long.valueOf(0L), aloneRun.get("unit"), "not in a unit"),
					() -> assertEquals(Long.valueOf(0L), aloneRun.get("leg")),
					() -> assertNotEquals(Long.valueOf(0L), firstLeg.get("unit"), "in a unit"),
					() -> assertEquals(firstLeg.get("unit"), secondLeg.get("unit"), "the same unit"),
					() -> assertEquals(Long.valueOf(1L), firstLeg.get("leg")),
					() -> assertEquals(Long.valueOf(2L), secondLeg.get("leg")),
					() -> assertEquals(SailMedia.SESSION, secondLeg.get("session")),
					() -> assertEquals("Interaction", secondLeg.get("title"), "titled by its interaction"));
	}

	private static Map<String, Object> runJson(Path run) throws IOException {
		return new Json().toType(Files.readString(run.resolve(SailMedia.RUN_JSON)), Json.MAP_TYPE);
	}

	@Test
	void aStepIsSummarisedInWords() {
		assertAll(() -> assertEquals("Wait for: !! document.querySelector('.home')",
										SailMedia.summary("<waitFor timeoutMillis=\"30000\"> <![CDATA[ return !! document.querySelector('.home'); ]]> </waitFor>")),
					() -> assertEquals("Wait for: a && b", SailMedia.summary("<waitFor timeoutMillis=\"60000\" script=\"return a && b;\" />"),
										"an executor's description, which is not well-formed XML"),
					() -> assertEquals(Map.of("user", "a", "script", "x => y < 2"),
										SailMedia.describedAttributes("<login user=\"a\" script=\"x => y < 2\" =\"nameless\" unclosed=\"z"),
										"a description's attributes: a value may hold '>', a nameless or unclosed one is skipped"),
					() -> assertEquals("Check: the header says offline", SailMedia.summary("<testScript message=\"the header says offline\">return x;</testScript>")),
					() -> assertEquals("Check: x === 1", SailMedia.summary("<testScript>return x === 1;</testScript>")),
					() -> assertEquals("Lower quota (headroomKb: 512)",
										SailMedia.summary("<call className=\"sail.inspection.LowerQuota\"> <parameter name=\"headroomKb\" value=\"512\"/> </call>")),
					() -> assertEquals("Switch tab (into tabs)", SailMedia.summary("<call className=\"sail.inspection.SwitchTab\" variable=\"tabs\"/>")),
					() -> assertEquals("Run: location.reload()", SailMedia.summary("<execute>location.reload();</execute>")),
					() -> assertEquals("Sign in as sail.pm, remembered",
										SailMedia.summary("<login password=\"{var:password}\" remember=\"true\" user=\"sail.pm\"/>")),
					() -> assertEquals("Sign out", SailMedia.summary("<logout/>")),
					() -> assertEquals("Go to basix/a.xhtml?i=1&inspect", SailMedia.summary("<navigateLink href=\"basix/a.xhtml?i=1&amp;inspect\"/>")),
					() -> assertEquals("Discard is offered only online", SailMedia.summary("<comment>Discard is offered only online</comment>")),
					() -> assertEquals("Pause 5000 ms", SailMedia.summary("<pause millis=\"5000\"/>")),
					() -> assertEquals("Navigate edit (bizId: 1, document: Inspection, module: tenancy)",
										SailMedia.summary("<navigateEdit bizId=\"1\" document=\"Inspection\" module=\"tenancy\"/>")),
					() -> assertEquals("Test success", SailMedia.summary("<testSuccess/>")),
					() -> assertEquals("Wait for: " + "x".repeat(139) + '\u2026', SailMedia.summary("<waitFor>" + "x".repeat(200) + "</waitFor>"),
										"a long script is shortened"));
	}

	@Test
	void namesAndDurationsAreInWords() {
		assertAll(() -> assertEquals("Replace front elevation", SailMedia.words("ReplaceFrontElevation")),
					() -> assertEquals("Check URL bar", SailMedia.words("CheckURLBar")),
					() -> assertEquals("Execute async", SailMedia.words("executeAsync")),
					() -> assertEquals("250 ms", SailMedia.duration(250L)),
					() -> assertEquals("1.3 s", SailMedia.duration(1250L)),
					() -> assertEquals("2 min 5 s", SailMedia.duration(125_000L)));
	}

	@Test
	void aRunIsTitledByItsFirstInteraction() {
		Automation automation = new Automation();
		assertEquals("T.t", SailMedia.title(automation, "T.t"), "no interactions: the test");
		Interaction first = new Interaction();
		first.setName("HM7 Home offline lists what the device holds");
		Interaction second = new Interaction();
		second.setName("HM7 office");
		automation.getInteractions().add(first);
		assertEquals("HM7 Home offline lists what the device holds", SailMedia.title(automation, "T.t"));
		automation.getInteractions().add(second);
		assertEquals("HM7 Home offline lists what the device holds (+1 more)", SailMedia.title(automation, "T.t"));
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
