package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.Select;
import org.openqa.selenium.support.ui.WebDriverWait;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import jakarta.annotation.Nonnull;
import util.sail.SailMedia.Frame;
import util.sail.SailMedia.Place;
import util.sail.SailMedia.RunInfo;
import util.sail.SailMedia.StepRecord;
import util.sail.SailMedia.Story;

/**
 * Uses the generated SAIL media pages in a headless Chrome. The video player: the step list, a click on a step,
 * stepping with the buttons and the arrow keys, seeking, and playing to the end; its run has a step during which no
 * frame was drawn, which must still be the step shown when it is clicked (it once showed the step before). The list of
 * runs: its "Failed only" and text filters, which hide the runs, headings and sessions with nothing to show, and
 * match the suite's, workflow's and runs' descriptions. The pages come from a JDK {@link HttpServer}, so this needs
 * no Skyve server; it needs Chrome. The frames are not real images: the test reads which one the player shows from
 * its source.
 */
@Tag("sail")
@SuppressWarnings("java:S5960") // test-only assertions
class SailMediaPagesSailIT {
	private static final Story NO_STORY = new Story(null, null, null, null);
	private static final RunInfo INFO = new RunInfo("20261106-120000-000-PlayerIT.test", "Open and save", "PlayerIT.test", null,
													"2026-11-06 12:00:00", "desktop", List.of(), NO_STORY);
	/** Step 2 runs from 2000 to 3000 ms with no frame drawn during it. */
	private static final List<StepRecord> STEPS = List.of(
			new StepRecord(1, "Open", 1, "Go to a.xhtml", "<navigateLink href=\"a.xhtml\"/>", 1000L, 2000L, null, null, null, null),
			new StepRecord(2, "Open", 2, "Pause 1000 ms", "<pause millis=\"1000\"/>", 2000L, 3000L, null, null, null, null),
			new StepRecord(3, "Save", 1, "Save", "<save/>", 3000L, 4000L, null, null, null, null));
	private static final List<Frame> FRAMES = List.of(new Frame("f0.png", 500L),
														new Frame("f1.png", 1200L),
														new Frame("f2.png", 1500L),
														new Frame("f3.png", 3100L),
														new Frame("f4.png", 3900L));

	@Nonnull private final PrimeFacesSelenide selenium = new PrimeFacesSelenide();
	@TempDir private Path root;
	private HttpServer server;
	private String url;
	private WebDriver driver;

	@BeforeEach
	void startTheServerAndBrowser() throws IOException {
		server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
		server.createContext("/", this::serve);
		server.start();
		url = "http://localhost:" + server.getAddress().getPort() + '/';
		selenium.startBrowser(new BrowserConfiguration().browser(BrowserConfiguration.Browsers.chrome).baseUrl(url).fixed().headless(true));
		driver = selenium.driver;
	}

	@AfterEach
	void cleanUp() {
		selenium.stopBrowser();
		SailServerLock.releaseAll();
		server.stop(0);
	}

	/** Serves the pages written under the root; anything else (the frames) is not found. */
	private void serve(@Nonnull HttpExchange exchange) throws IOException {
		String name = exchange.getRequestURI().getPath().substring(1);
		Path file = root.resolve(name).normalize();
		boolean found = file.startsWith(root) && Files.isRegularFile(file);
		byte[] body = found ? Files.readAllBytes(file) : new byte[0];
		exchange.getResponseHeaders().add("Content-Type", name.endsWith(".js") ? "text/javascript; charset=utf-8" : "text/html; charset=utf-8");
		exchange.sendResponseHeaders(found ? 200 : 404, (body.length == 0) ? -1 : body.length);
		if (body.length > 0) {
			try (OutputStream out = exchange.getResponseBody()) {
				out.write(body);
			}
		}
	}

	private void openThePlayer() throws IOException {
		Files.writeString(root.resolve("player.html"), SailRunPages.playerHtml(INFO, STEPS, null));
		Files.writeString(root.resolve("video.js"), SailRunPages.videoScript(INFO, FRAMES, STEPS, null));
		driver.get(url + "player.html");
	}

	@Test
	void theStepsAreListedInWordsByInteraction() throws IOException {
		openThePlayer();
		assertAll(() -> assertEquals(List.of("Open", "Save"), texts("#steps h3")),
					() -> assertEquals(List.of("Go to a.xhtml", "Pause 1000 ms", "Save"), texts("#steps li .summary")),
					() -> assertEquals("<pause millis=\"1000\"/>", steps().get(1).getDomAttribute("title"), "the step's XML on hover"),
					() -> assertEquals("f0.png", frame(), "the first frame"),
					() -> assertEquals("Before the first step", caption()));
	}

	@Test
	void aClickedStepIsTheStepShown() throws IOException {
		openThePlayer();
		steps().get(2).click();
		assertShown(3, "f4.png", "a step as it finished: the last frame drawn by its end, not f3 as it started");
		steps().get(1).click();
		assertShown(2, "f2.png", "no frame was drawn during the step: the screen as it was, and still this step");
		steps().get(0).click();
		assertShown(1, "f2.png", "the first step as it finished, not f1 as it started");
	}

	@Test
	void theButtonsAndArrowKeysStepThroughTheSteps() throws IOException {
		openThePlayer();
		driver.findElement(By.id("next")).click();
		assertShown(1, "f2.png", "next from before the first step: the step as it finished");
		driver.findElement(By.id("next")).click();
		assertShown(2, "f2.png", "next");
		driver.findElement(By.tagName("body")).sendKeys(Keys.ARROW_RIGHT);
		assertShown(3, "f4.png", "the right arrow");
		driver.findElement(By.id("next")).click();
		assertShown(3, "f4.png", "no step after the last");
		driver.findElement(By.tagName("body")).sendKeys(Keys.ARROW_LEFT);
		assertShown(2, "f2.png", "the left arrow");
		driver.findElement(By.id("prev")).click();
		assertShown(1, "f2.png", "previous");
	}

	@Test
	void seekingShowsTheStepRunningAtThatFrame() throws IOException {
		openThePlayer();
		steps().get(2).click();
		seek(2);
		assertShown(1, "f2.png", "a seek lets go of the clicked step: the step running at 1500 ms");
		seek(0);
		assertEquals("Before the first step", caption());
	}

	@Test
	void playingAfterAJumpGoesOnFromTheJumpedToStep() throws IOException {
		openThePlayer();
		new Select(driver.findElement(By.id("speed"))).selectByValue("8");
		steps().get(0).click();
		assertShown(1, "f2.png", "the first step as it finished");
		// note each frame shown from now on, to tell going on from the step apart from starting again
		((JavascriptExecutor) driver).executeScript("var img = document.getElementById('frame'); window.sailShown = [];" +
														"new MutationObserver(function () { window.sailShown.push(img.getAttribute('src')); })" +
														".observe(img, {attributes: true, attributeFilter: ['src']});");
		WebElement play = driver.findElement(By.id("play"));
		play.click();
		new WebDriverWait(driver, Duration.ofSeconds(10)).until(d -> Boolean.valueOf("Play".equals(play.getText()) && "f4.png".equals(frame())));
		Object shown = ((JavascriptExecutor) driver).executeScript("return window.sailShown.join(',');");
		assertAll(() -> assertShown(3, "f4.png", "played to the end"),
					() -> assertEquals("f3.png,f4.png", shown, "on from the step's frame, not from the start"));
	}

	@Test
	void playingRunsToTheEndAndStops() throws IOException {
		openThePlayer();
		new Select(driver.findElement(By.id("speed"))).selectByValue("8");
		WebElement play = driver.findElement(By.id("play"));
		play.click();
		new WebDriverWait(driver, Duration.ofSeconds(10)).until(d -> Boolean.valueOf("Play".equals(play.getText()) && "f4.png".equals(frame())));
		assertAll(() -> assertShown(3, "f4.png", "the last frame, in the last step"),
					() -> assertEquals("4", driver.findElement(By.id("seek")).getDomProperty("value"), "the position at the end"),
					() -> assertEquals("3.4 s / 3.4 s", driver.findElement(By.id("clock")).getText()));
	}

	/**
	 * Writes a list of two sessions: the newer has a suite file with a run and a workflow of two legs (the second
	 * failed), then a second suite file with one run; the older has one run.
	 */
	private void openTheListOfRuns() throws IOException {
		Path runs = Files.createDirectories(root.resolve("runs"));
		Place none = new Place("20261106-115900-000", 0, 0);
		listedRun(runs, "20261106-085900-000-A.test", "A1 earlier session", null, new Place("20261106-085800-000", 0, 0), true);
		listedRun(runs, "20261106-120000-000-B.test", "HM1 home lists inspections", "10-home.xml", none, true);
		listedRun(runs, "20261106-120100-000-B.test", "HM2 phone: works offline", "10-home.xml", new Place(none.session(), 7, 1), true);
		listedRun(runs, "20261106-120200-000-B.test", "HM2 office: recalls it", "10-home.xml", new Place(none.session(), 7, 2), false);
		listedRun(runs, "20261106-120300-000-B.test", "CK1 checks out", "20-checkout.xml", none, true);
		Files.writeString(runs.resolve("index.html"), SailRunsList.runsIndexHtml(SailRunsList.listedRuns(runs)));
		driver.get(url + "runs/index.html");
	}

	private static void listedRun(Path base, String folder, String title, String label, Place place, boolean passed) throws IOException {
		listedRun(base, folder, title, label, place, passed, NO_STORY);
	}

	private static void listedRun(Path base, String folder, String title, String label, Place place, boolean passed, Story story)
	throws IOException {
		Path run = Files.createDirectories(base.resolve(folder));
		Files.writeString(run.resolve("player.html"), "player");
		RunInfo info = new RunInfo(folder, title, "B.test", label, "2026-11-06 12:00:00", "phone", List.of(), story);
		List<StepRecord> steps = List.of(new StepRecord(1, title, 1, "Save", "<save/>", 0L, 10L, passed ? null : "it failed", null, null, null));
		Files.writeString(run.resolve(SailMedia.RUN_JSON), SailMedia.runJson(info, place, steps, passed));
	}

	@Test
	void failedOnlyShowsTheFailedRunsUnderTheirHeadings() throws IOException {
		openTheListOfRuns();
		driver.findElement(By.id("failedOnly")).click();
		assertAll(() -> assertEquals(List.of("HM2 office: recalls it"), shownRuns()),
					() -> assertEquals(List.of("10-home.xml", "HM2"), shownHeadings(), "its file and its workflow"),
					() -> assertEquals(1L, shownSessions(), "the older session has no failed run"),
					() -> assertEquals("1 shown", driver.findElement(By.id("shown")).getText()));
		driver.findElement(By.id("failedOnly")).click();
		assertAll("cleared",
					() -> assertEquals(5, shownRuns().size()),
					() -> assertEquals(2L, shownSessions()),
					() -> assertEquals("", driver.findElement(By.id("shown")).getText()));
	}

	@Test
	void theTextFilterMatchesARunOrItsHeadings() throws IOException {
		openTheListOfRuns();
		WebElement find = driver.findElement(By.id("find"));
		find.sendKeys("checkout");
		assertAll("a file heading's text",
					() -> assertEquals(List.of("CK1 checks out"), shownRuns()),
					() -> assertEquals(List.of("20-checkout.xml"), shownHeadings()),
					() -> assertEquals(1L, shownSessions()));
		find.clear();
		find.sendKeys("workflow");
		assertAll("a workflow heading's text",
					() -> assertEquals(List.of("HM2 phone: works offline", "HM2 office: recalls it"), shownRuns()),
					() -> assertEquals(List.of("10-home.xml", "HM2"), shownHeadings()),
					() -> assertEquals("2 shown", driver.findElement(By.id("shown")).getText()));
		find.clear();
		find.sendKeys("earlier");
		assertAll("a run's own text, in the older session",
					() -> assertEquals(List.of("A1 earlier session"), shownRuns()),
					() -> assertEquals(List.of(), shownHeadings()),
					() -> assertEquals(1L, shownSessions()));
	}

	/**
	 * Writes a list of one session: a suite with a description, whose member file has a run and a workflow of two legs,
	 * each with descriptions; then a run outside the suite.
	 */
	private void openTheListOfStories() throws IOException {
		Path runs = Files.createDirectories(root.resolve("stories"));
		Place none = new Place("20261120-115900-000", 0, 0);
		String suite = "Inspections end to end.\n\nRuns on a phone and in the office.";
		listedRun(runs, "20261120-120000-000-B.test", "HM1 home lists inspections", "10-home.xml", none, true,
					new Story("insp.suite.xml", suite, null, "Lists the day's inspections."));
		listedRun(runs, "20261120-120100-000-B.test", "HM2 phone: works offline", "10-home.xml", new Place(none.session(), 7, 1), true,
					new Story("insp.suite.xml", suite, "An inspector works offline and the office recalls it.", "Records it offline."));
		listedRun(runs, "20261120-120200-000-B.test", "HM2 office: recalls it", "10-home.xml", new Place(none.session(), 7, 2), true,
					new Story("insp.suite.xml", suite, "An inspector works offline and the office recalls it.", "Recalls it."));
		listedRun(runs, "20261120-120300-000-B.test", "CK1 checks out", null, none, true);
		Files.writeString(runs.resolve("index.html"), SailRunsList.runsIndexHtml(SailRunsList.listedRuns(runs)));
		driver.get(url + "stories/index.html");
	}

	/**
	 * The text filter matches a suite's, a workflow's and a run's descriptions, and text that runs on from one paragraph
	 * to the next; the suite's heading shows while any of its runs does.
	 */
	@Test
	void theTextFilterMatchesTheDescriptions() throws IOException {
		openTheListOfStories();
		WebElement find = driver.findElement(By.id("find"));
		find.sendKeys("inspections end to end");
		assertAll("the suite's description",
					() -> assertEquals(List.of("HM1 home lists inspections", "HM2 phone: works offline", "HM2 office: recalls it"), shownRuns()),
					() -> assertTrue(shownHeadings().get(0).startsWith("Suite insp.suite.xml"), shownHeadings().toString()));
		find.clear();
		find.sendKeys("end to end. runs on");
		assertEquals(3, shownRuns().size(), "across the suite's two paragraphs");
		find.clear();
		find.sendKeys("office recalls");
		assertEquals(List.of("HM2 phone: works offline", "HM2 office: recalls it"), shownRuns(), "the workflow's description");
		find.clear();
		find.sendKeys("day's inspections");
		assertAll("a run's own description",
					() -> assertEquals(List.of("HM1 home lists inspections"), shownRuns()),
					() -> assertEquals(2, shownHeadings().size(), "the suite and its file; not the workflow, which has no run shown"));
		find.clear();
		find.sendKeys("checks out");
		assertAll("a run outside the suite: no suite heading",
					() -> assertEquals(List.of("CK1 checks out"), shownRuns()),
					() -> assertEquals(List.of(), shownHeadings()));
	}

	/**
	 * A run page's previous and next run buttons follow the list's order, across sessions, to the neighbour's page of
	 * the same kind (else its other page), and show the run's place in its session; the first run has no previous, the
	 * last no next. The pages work opened from the file system too.
	 */
	@Test
	void theRunButtonsGoToTheNeighboursPages() throws IOException {
		Path runs = Files.createDirectories(root.resolve("nav"));
		Place newer = new Place("20261120-115900-000", 0, 0);
		SailRunNavTest.run(runs, "20261120-120000-000-B.test", "R1 both", newer, true, true, true);
		SailRunNavTest.run(runs, "20261120-120100-000-B.test", "R2 both", newer, true, true, false);
		SailRunNavTest.run(runs, "20261120-120200-000-B.test", "R3 video only", newer, false, true, true);
		SailRunNavTest.run(runs, "20261120-090000-000-A.test", "R0 older, steps only", new Place("20261120-085900-000", 0, 0), true, false, true);
		List<SailRunsList.ListedRun> listed = SailRunsList.listedRuns(runs);
		Files.writeString(runs.resolve("index.html"), SailRunsList.runsIndexHtml(listed));
		Files.writeString(runs.resolve(SailMedia.RUNS_ORDER_JS), SailRunsList.runsOrderScript(listed));

		driver.get(url + "nav/20261120-120100-000-B.test/index.html");
		WebElement previous = driver.findElement(By.id("prevRun"));
		WebElement next = driver.findElement(By.id("nextRun"));
		assertAll("the middle run's film strip",
					() -> assertTrue(previous.isDisplayed() && next.isDisplayed()),
					() -> assertTrue(previous.getDomAttribute("href").endsWith("/20261120-120000-000-B.test/index.html"), previous.getDomAttribute("href")),
					() -> assertTrue(next.getDomAttribute("href").endsWith("/20261120-120200-000-B.test/player.html"), "a neighbour with no film strip: its player"),
					() -> assertEquals("Run 2 of 3", driver.findElement(By.id("runPos")).getText(), "its place in its session"),
					() -> assertTrue(previous.getDomAttribute("title").startsWith("Previous run: R1 both"), previous.getDomAttribute("title")));
		next.click();
		assertAll("the session's last run's player, by the button",
					() -> assertTrue(driver.getCurrentUrl().endsWith("/20261120-120200-000-B.test/player.html"), driver.getCurrentUrl()),
					() -> assertTrue(driver.findElement(By.id("prevRun")).getDomAttribute("href").endsWith("/20261120-120100-000-B.test/player.html"),
										"a video page goes to the neighbour's video"),
					() -> assertTrue(driver.findElement(By.id("nextRun")).getDomAttribute("href").endsWith("/20261120-090000-000-A.test/index.html"),
										"on into the older session, to its film strip as it has no video"),
					() -> assertEquals("Run 3 of 3", driver.findElement(By.id("runPos")).getText()));
		driver.get(runs.resolve("20261120-090000-000-A.test").resolve("index.html").toUri().toString());
		assertAll("the older session's only run, opened from the file system",
					() -> assertTrue(driver.findElement(By.id("prevRun")).getDomAttribute("href").endsWith("/20261120-120200-000-B.test/player.html")),
					() -> assertFalse(driver.findElement(By.id("nextRun")).isDisplayed(), "the last run has no next run"),
					() -> assertEquals("Run 1 of 1", driver.findElement(By.id("runPos")).getText(), "numbered in its own session"));
		driver.get(url + "nav/20261120-120000-000-B.test/player.html");
		assertFalse(driver.findElement(By.id("prevRun")).isDisplayed(), "the first run has no previous run");
	}

	/** A run page with no runs' order beside its list (written before the order existed) shows no buttons and no place. */
	@Test
	void aRunPageWithoutTheOrderShowsNoButtons() throws IOException {
		Path runs = Files.createDirectories(root.resolve("unordered"));
		SailRunNavTest.run(runs, "20261120-120000-000-B.test", "R1", new Place("20261120-115900-000", 0, 0), true, false, true);
		driver.get(url + "unordered/20261120-120000-000-B.test/index.html");
		assertAll(() -> assertFalse(driver.findElement(By.id("prevRun")).isDisplayed()),
					() -> assertFalse(driver.findElement(By.id("nextRun")).isDisplayed()),
					() -> assertFalse(driver.findElement(By.id("runPos")).isDisplayed()),
					() -> assertTrue(driver.findElement(By.cssSelector("a[href='../index.html']")).isDisplayed(), "All runs still shows"));
	}

	/** The titles of the runs shown, in order. */
	private List<String> shownRuns() {
		return driver.findElements(By.cssSelector("tr[data-run]")).stream().filter(WebElement::isDisplayed)
						.map(row -> row.findElement(By.cssSelector(".summary")).getText()).toList();
	}

	/** The file and workflow headings shown, in order (a workflow by its title). */
	private List<String> shownHeadings() {
		return driver.findElements(By.cssSelector("tr[data-header]")).stream().filter(WebElement::isDisplayed)
						.map(row -> row.findElements(By.cssSelector(".summary")).isEmpty() ? row.getText()
																						: row.findElement(By.cssSelector(".summary")).getText())
						.toList();
	}

	private long shownSessions() {
		return driver.findElements(By.cssSelector("section.session")).stream().filter(WebElement::isDisplayed).count();
	}

	private void assertShown(int step, String frame, String what) {
		assertAll(what,
					() -> assertEquals(frame, frame(), "the frame"),
					() -> assertEquals(List.of(STEPS.get(step - 1).xml()),
										driver.findElements(By.cssSelector("#steps li.current")).stream().map(li -> li.getDomAttribute("title")).toList(),
										"the highlighted step"),
					() -> assertEquals("Step " + step + " of 3 · 1.0 s", driver.findElement(By.cssSelector("#caption .cap-meta span:last-child")).getText(),
										"the caption"));
	}

	private List<WebElement> steps() {
		return driver.findElements(By.cssSelector("#steps li"));
	}

	private List<String> texts(String css) {
		return driver.findElements(By.cssSelector(css)).stream().map(WebElement::getText).toList();
	}

	private String frame() {
		return driver.findElement(By.id("frame")).getDomAttribute("src");
	}

	private String caption() {
		return driver.findElement(By.id("caption")).getText();
	}

	/** Moves the position slider, as dragging it does. */
	private void seek(int frame) {
		((JavascriptExecutor) driver).executeScript(
				"var s = document.getElementById('seek'); s.value = arguments[0]; s.dispatchEvent(new Event('input'));", Integer.valueOf(frame));
	}
}
