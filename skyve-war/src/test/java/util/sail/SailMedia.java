package util.sail;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.json.Json;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import util.sail.browser.Capability;
import util.sail.browser.PageScreencast;
import util.sail.browser.SailBrowserControl;

/**
 * Step snapshots and a video of one automation run, for a person to look through what a SAIL run did.
 * <p>
 * Both are opt-in, by {@link SailSettings} (a directory, or an evidence request); without them {@link #start} returns
 * null and the executor does nothing more.
 * <ul>
 * <li>{@value SailSettings#SNAPSHOT_DIR}: after each step (and when a step fails, before the test strategy acts on
 * it), a screenshot named by sequence, interaction and step, and an {@code index.html} film strip of the steps, under
 * their interactions: each step in words ({@link SailStepWords#summary(Step)}) and as SAIL, its outcome and duration, and its
 * screenshot. With {@value SailSettings#SNAPSHOT_SOURCE}{@code =true}, also each page's source.</li>
 * <li>{@value SailSettings#VIDEO_DIR}: the browser's frames, from Chrome's DevTools screencast, with their times,
 * and a {@code player.html} that plays them at their real timing, fitted to the window, captioned with the step that
 * was running, beside a list of the steps in words to jump to.</li>
 * </ul>
 * Each automation run gets its own folder under each directory, named by its time, test and (for a suite file) the
 * script, so suites, workflows and ITs with several tests keep every run, with a {@value #RUN_JSON} summary of it.
 * Each directory gets an {@code index.html} listing its runs by what they do (their first interaction), with their
 * outcomes, and filters, and a {@value #RUNS_ORDER_JS} of the runs in that order, from which each run's pages make
 * their previous and next run buttons. The default directory's list is one folder up, in the evidence folder beside
 * {@code media/} and {@code failures/} (see {@link SailSettings#runsListDir(Path)}), so that it is the page to open. Everything is static HTML that opens from the file system, light or dark as the system is.
 * This class records the run; {@link SailRunPages} writes the film strip and player, {@link SailRunsList} the list of
 * runs, {@link SailStepWords} each step's words, and {@link SailMediaHtml} holds their shared parts and the CSS and
 * JavaScript (in {@code util/sail/media/}).
 * The pages tell the run's story from its script's descriptions ({@link Story}): the suite's (see
 * {@link #suite(String, String)}), the workflow's (see {@link #beginUnit(String)}), the automation's and each
 * interaction's.
 * The run's pages have a Settings button that shows its settings that do not come from their defaults, with their
 * sources. With {@value SailSettings#KEEP_MEDIA}{@code =failed}, a run that passed deletes its folders when it ends.
 * <p>
 * Recordings show what the browser showed: a run against real data records real data.
 * <p>
 * Threading: an instance belongs to the thread running its automation. Screencast frames arrive on the browser
 * control's DevTools thread, which acknowledges each without waiting and hands it to a single worker thread to write,
 * so the test thread never waits for them until the run ends.
 */
public final class SailMedia {
	/**
	 * System property naming a directory for step snapshots and their film strip.
	 * @deprecated	use {@link SailSettings#SNAPSHOT_DIR}
	 */
	@Deprecated(since = "10")
	public static final String SNAPSHOT_DIRECTORY_PROPERTY = SailSettings.SNAPSHOT_DIR;
	/**
	 * System property that, when {@code true}, also saves each step's page source with its snapshot.
	 * @deprecated	use {@link SailSettings#SNAPSHOT_SOURCE}
	 */
	@Deprecated(since = "10")
	public static final String SNAPSHOT_SOURCE_PROPERTY = SailSettings.SNAPSHOT_SOURCE;
	/**
	 * System property naming a directory for the run's video.
	 * @deprecated	use {@link SailSettings#VIDEO_DIR}
	 */
	@Deprecated(since = "10")
	public static final String VIDEO_DIRECTORY_PROPERTY = SailSettings.VIDEO_DIR;

	/** The screencast's image format, quality and largest frame: enough to read a page, small enough to keep many. */
	static final String FRAME_FORMAT = "jpeg";
	static final int FRAME_QUALITY = 60;
	static final int FRAME_MAX_WIDTH = 1280;
	static final int FRAME_MAX_HEIGHT = 800;

	private static final DateTimeFormatter FOLDER_TIME = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss-SSS");
	private static final DateTimeFormatter SHOWN_TIME = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
	private static final Pattern METHOD_CONTENT = Pattern.compile("<(?:\\w+:)?method>(.*)</(?:\\w+:)?method>", Pattern.DOTALL);
	/** Each run folder's summary of the run, which the list of runs reads (see {@link #runJson}). */
	static final String RUN_JSON = "run.json";
	/** Each directory's runs in the list's order, beside its list, for the run pages' previous and next run buttons. */
	static final String RUNS_ORDER_JS = "runs-order.js";
	/** The script a suite is running on this thread, to name the run's folder; see {@link #label(String)}. */
	private static final ThreadLocal<String> LABEL = new ThreadLocal<>();
	/** The suite running on this thread, for its runs' pages; see {@link #suite(String, String)}. */
	private static final ThreadLocal<SuiteStory> SUITE = new ThreadLocal<>();
	/**
	 * The unit (a workflow) running on this thread; null outside a unit. Set by {@link #beginUnit(String)} and removed by
	 * {@link #endUnit(boolean)}, which its caller calls in a {@code finally}.
	 */
	private static final ThreadLocal<Unit> UNIT = new ThreadLocal<>();
	/**
	 * This JVM's run of tests, for the list of runs to show each session's runs together in the order they ran: when
	 * this class was loaded, as a folder time.
	 */
	static final String SESSION = FOLDER_TIME.format(LocalDateTime.now());
	/** Numbers the units (see {@link #beginUnit(String)}) of this session. */
	private static final AtomicInteger UNITS = new AtomicInteger();

	/**
	 * A unit (a workflow): its number in the session, its description, how many runs (legs) it has started, and its
	 * finished runs.
	 */
	private static final class Unit {
		private final int number = UNITS.incrementAndGet();
		private final String description;
		private int legs = 0;
		/** Its finished runs, whose media is kept or deleted together. */
		private final List<SailMedia> runs = new ArrayList<>();

		private Unit(@Nullable String description) {
			this.description = description;
		}
	}

	/** The suite a run is a member of: its file, and its description. */
	private record SuiteStory(@Nonnull String name, @Nullable String description) {
		// a value
	}

	/**
	 * What the run's script says it is for, in words: each a {@code <description>}, tidied (see
	 * {@link org.skyve.metadata.sail.language.Descriptions}); null where there is none.
	 *
	 * @param suite	the suite the run is a member of, or null
	 * @param suiteDescription	the suite's description
	 * @param workflowDescription	the description of the workflow the run is a leg of
	 * @param description	the automation's description
	 */
	record Story(@Nullable String suite,
					@Nullable String suiteDescription,
					@Nullable String workflowDescription,
					@Nullable String description) {
	}

	/**
	 * Where a run is in its session: the unit (a workflow) it is a leg of, and which leg.
	 *
	 * @param session	the session ({@link #SESSION})
	 * @param unit	the unit's number in the session, or 0 when the run is not in a unit
	 * @param leg	the run's number in its unit, from 1; or 0 when it is not in a unit
	 */
	record Place(@Nonnull String session, int unit, int leg) {
		// a value
	}

	/**
	 * One executed step: where it ran (and what its interaction is for, in words), what it was (in words, see
	 * {@link SailStepWords#summary(Step)}, and as SAIL), when, how it went, and its snapshot.
	 */
	record StepRecord(int sequence,
						@Nonnull String procedure,
						int number,
						@Nonnull String summary,
						@Nonnull String xml,
						long startMillis,
						long endMillis,
						@Nullable String failure,
						@Nullable String screenshot,
						@Nullable String source,
						@Nullable String procedureDescription) {
		boolean failed() {
			return failure != null;
		}
	}

	/** One screencast frame: its file, relative to the run's video folder, and when Chrome drew it. */
	record Frame(@Nonnull String file, long millis) {
		// a value
	}

	/**
	 * What a run's pages say about the run.
	 *
	 * @param name	the run's folder name
	 * @param title	what the run does: its automation's first interaction (see {@link SailMedia#title(Automation, String)})
	 * @param test	the test, as {@code Class.method}
	 * @param label	the suite file, or null
	 * @param started	when it started, as shown
	 * @param uxui	the automation's UX/UI
	 * @param settings	its settings that do not come from their defaults, each with its source (see {@link SailSettings#describe()})
	 * @param story	what its suite, workflow and automation say they are for
	 */
	record RunInfo(@Nonnull String name,
					@Nonnull String title,
					@Nonnull String test,
					@Nullable String label,
					@Nonnull String started,
					@Nonnull String uxui,
					@Nonnull List<String> settings,
					@Nonnull Story story) {
	}

	private final Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide;
	private final Automation automation;
	private final RunInfo info;
	private final Place place;
	private final Path snapshotRun;
	private final Path videoRun;
	/** Where the snapshot and video directories' lists of runs go (see {@link SailSettings#runsListDir(Path)}). */
	private final Path snapshotList;
	private final Path videoList;
	private final boolean saveSource;
	private final SailSettings.KeepMedia keepMedia;
	/** Whether the run passed with no failed step, once it has finished. */
	private boolean passed;
	private final List<StepRecord> steps = new ArrayList<>();
	private Screencast screencast;
	/** The browser the screencast was last started for (or tried), so that it is restarted only for a new browser. */
	private WebDriver screencastDriver;
	/** The run's video frames, in order, across screencasts; guarded by itself. */
	private final List<Frame> frames = new ArrayList<>();

	// the step that is running
	private int sequence = 0;
	private String procedure;
	private String procedureDescription;
	private int number;
	private String stepSummary;
	private String xml;
	private String fileStem;
	private long startMillis;

	private SailMedia(@Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide,
						@Nonnull Automation automation,
						@Nonnull RunInfo info,
						@Nullable Path snapshotRun,
						@Nullable Path videoRun,
						@Nonnull SailSettings settings) {
		this.selenide = selenide;
		this.automation = automation;
		this.info = info;
		Unit unit = UNIT.get();
		this.place = (unit == null) ? new Place(SESSION, 0, 0) : new Place(SESSION, unit.number, ++unit.legs);
		this.snapshotRun = snapshotRun;
		this.videoRun = videoRun;
		this.snapshotList = (snapshotRun == null) ? null : settings.runsListDir(snapshotRun.getParent());
		this.videoList = (videoRun == null) ? null : settings.runsListDir(videoRun.getParent());
		this.saveSource = settings.snapshotSource();
		this.keepMedia = settings.keepMedia();
	}

	/**
	 * Names the script that the runs started on this thread come from, for their folders; a suite sets it for each
	 * file. {@code null} clears it.
	 *
	 * @param label	the script, or null
	 */
	public static void label(@Nullable String label) {
		if (label == null) {
			LABEL.remove();
		}
		else {
			LABEL.set(label);
		}
	}

	/**
	 * Starts snapshots and video for an automation run, as the settings ask.
	 *
	 * <p>Side effects: makes the run's folders.
	 *
	 * @param selenide	the browser the automation drives
	 * @param automation	the automation
	 * @param settings	the run's settings
	 * @return the run's media, or null when the settings give neither a snapshot nor a video directory
	 */
	static @Nullable SailMedia start(@Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide,
										@Nonnull Automation automation,
										@Nonnull SailSettings settings) {
		Path snapshotBase = settings.snapshotDir();
		Path videoBase = settings.videoDir();
		if ((snapshotBase == null) && (videoBase == null)) {
			return null;
		}

		LocalDateTime now = LocalDateTime.now();
		String test = testName();
		String label = LABEL.get();
		// a load run's virtual users start runs at the same time: their folders name the user and iteration
		String name = sanitise(FOLDER_TIME.format(now) + '-' + SailLoadProbe.fileLabel() + test + ((label == null) ? "" : '-' + label), 120);
		RunInfo info = new RunInfo(name, title(automation, test), test, label, SHOWN_TIME.format(now), automation.getUxui(), settings.describe(),
									story(automation));
		try {
			SailMedia result = new SailMedia(selenide,
												automation,
												info,
												(snapshotBase == null) ? null : Files.createDirectories(snapshotBase.resolve(name)),
												(videoBase == null) ? null : Files.createDirectories(videoBase.resolve(name).resolve("frames")).getParent(),
												settings);
			selenide.trace("SAIL media for this run: " + ((snapshotBase == null) ? "" : "snapshots in " + result.snapshotRun + " ") +
							((videoBase == null) ? "" : "video in " + result.videoRun));
			return result;
		}
		catch (IOException e) {
			selenide.trace("Could not make the SAIL media folders, so this run has none: " + e);
			return null;
		}
	}

	/**
	 * Returns what the run's suite, workflow and automation say they are for: the suite and the unit (a workflow)
	 * running on this thread, and the automation.
	 *
	 * @param automation	the automation
	 * @return the story
	 */
	static @Nonnull Story story(@Nonnull Automation automation) {
		SuiteStory suite = SUITE.get();
		Unit unit = UNIT.get();
		return new Story((suite == null) ? null : suite.name(),
							(suite == null) ? null : suite.description(),
							(unit == null) ? null : unit.description,
							automation.getDescription());
	}

	/**
	 * Returns what a run does, for its pages and the list of runs: the name of its automation's first interaction, with
	 * how many more there are; or the test when it has none.
	 *
	 * @param automation	the automation
	 * @param test	the test, as {@code Class.method}
	 * @return the title
	 */
	static @Nonnull String title(@Nonnull Automation automation, @Nonnull String test) {
		List<String> names = automation.getInteractions().stream()
											.map(Interaction::getName)
											.filter(n -> (n != null) && (! n.isBlank()))
											.toList();
		if (names.isEmpty()) {
			return test;
		}
		int more = names.size() - 1;
		return names.get(0).trim() + ((more == 0) ? "" : " (+" + more + " more)");
	}

	/**
	 * Returns the test that is running the automation: the first caller outside the harness, Skyve, JUnit and the JDK,
	 * as {@code Class.method}.
	 */
	private static @Nonnull String testName() {
		return StackWalker.getInstance().walk(frames -> frames.filter(f -> {
			String type = f.getClassName();
			return ! (type.startsWith("util.") || type.startsWith("org.skyve.") || type.startsWith("java.") ||
						type.startsWith("jdk.") || type.startsWith("sun.") || type.startsWith("org.junit.") ||
						type.startsWith("org.apache.maven.") || type.startsWith("com.codeborne.") ||
						type.startsWith("org.openqa."));
		}).findFirst()
			.map(f -> {
				String type = f.getClassName();
				String method = f.getMethodName();
				// a lambda in a test method (lambda$test$0) is named for the test method
				if (method.startsWith("lambda$")) {
					int end = method.indexOf('$', 7);
					method = method.substring(7, (end < 0) ? method.length() : end);
				}
				return type.substring(type.lastIndexOf('.') + 1).replaceAll("\\$.*", "") + '.' + method;
			})
			.orElse("sail"));
	}

	/**
	 * Notes that a step starts, and starts or restarts the screencast if the browser has started or been replaced.
	 *
	 * @param procedureName	the interaction or procedure
	 * @param description	what the interaction is for, in words, or null
	 * @param stepNumber	the step's number in it, from 1
	 * @param step	the step
	 */
	void stepStarted(@Nonnull String procedureName, @Nullable String description, int stepNumber, @Nonnull Step step) {
		sequence++;
		procedure = procedureName;
		procedureDescription = description;
		number = stepNumber;
		try {
			stepSummary = SailStepWords.summary(step);
		}
		catch (RuntimeException e) { // the media must never change how a step fails: the step reports its own problem
			selenide.trace("Could not put step " + sequence + " in words for the SAIL media: " + e);
			stepSummary = SailStepWords.words(InterpretedWebDriverExecutor.opCode(step));
		}
		xml = stepXml(step);
		fileStem = String.format("%03d-%s", Integer.valueOf(sequence), sanitise(procedureName + '-' + InterpretedWebDriverExecutor.opCode(step), 80));
		if (videoRun != null) {
			WebDriver driver = selenide.driver;
			if ((driver != null) && (driver != screencastDriver)) {
				if (screencast != null) {
					screencast.stop();
				}
				screencastDriver = driver;
				screencast = Screencast.start(driver, videoRun, frames, selenide);
			}
		}
		startMillis = System.currentTimeMillis();
	}

	/**
	 * Notes that the running step ended, and takes its snapshot.
	 *
	 * @param failure	why it failed, or null when it passed
	 */
	void stepEnded(@Nullable String failure) {
		if (xml == null) {
			return; // no step is running (it has been ended already)
		}
		long endMillis = System.currentTimeMillis();
		String screenshot = null;
		String source = null;
		if (snapshotRun != null) {
			WebDriver driver = selenide.driver;
			try {
				if (driver instanceof TakesScreenshot camera) {
					screenshot = fileStem + ".png";
					Files.write(snapshotRun.resolve(screenshot), camera.getScreenshotAs(OutputType.BYTES));
				}
				if (saveSource && (driver != null)) {
					source = fileStem + ".html";
					Files.writeString(snapshotRun.resolve(source), driver.getPageSource(), StandardCharsets.UTF_8);
				}
			}
			catch (IOException | RuntimeException e) { // a closed browser or an unreadable page: the strip says so
				selenide.trace("Could not save the snapshot of step " + sequence + ": " + e);
				if ((screenshot != null) && (! Files.exists(snapshotRun.resolve(screenshot)))) {
					screenshot = null;
				}
				if ((source != null) && (! Files.exists(snapshotRun.resolve(source)))) {
					source = null;
				}
			}
		}
		steps.add(new StepRecord(sequence, procedure, number, stepSummary, xml, startMillis, endMillis,
									(failure == null) ? null : hidePasswords(failure), screenshot, source, procedureDescription));
		xml = null;
	}

	/**
	 * Names the suite that the runs started on this thread are members of, for their pages: its file, and what it is
	 * for. A suite sets it while it runs; {@code null} clears it.
	 *
	 * @param name	the suite's file, or null
	 * @param description	its description, or null
	 */
	static void suite(@Nullable String name, @Nullable String description) {
		if (name == null) {
			SUITE.remove();
		}
		else {
			SUITE.set(new SuiteStory(name, description));
		}
	}

	/**
	 * Returns the script that the runs started on this thread come from (see {@link #label(String)}).
	 *
	 * @return the label, or null outside a suite
	 */
	static @Nullable String label() {
		return LABEL.get();
	}

	/**
	 * Starts a unit of automation runs on this thread, such as a workflow, whose media is kept or deleted together: with
	 * {@value SailSettings#KEEP_MEDIA}{@code =failed}, a passed run's folders are deleted only when the whole unit
	 * passed (see {@link #endUnit(boolean)}). Outside a unit, each run decides for itself. The unit's description (a
	 * workflow's) is what its runs' pages show for it.
	 *
	 * <p>Side effects: sets this thread's unit, which the caller must end in a {@code finally}.
	 *
	 * @param description	the unit's description, or null
	 */
	static void beginUnit(@Nullable String description) {
		UNIT.set(new Unit(description));
	}

	/**
	 * Ends this thread's unit: if it passed, and every run in it passed, deletes the folders of its runs whose settings
	 * keep only failed runs' media.
	 *
	 * <p>Side effects: may delete the runs' folders and rewrite their directories' lists of runs; clears the unit.
	 *
	 * @param unitPassed	whether the unit passed
	 */
	static void endUnit(boolean unitPassed) {
		Unit unit = UNIT.get();
		UNIT.remove();
		if ((unit == null) || (! unitPassed)) {
			return;
		}
		List<SailMedia> runs = unit.runs;
		for (SailMedia run : runs) {
			if (! run.passed) {
				return;
			}
		}
		for (SailMedia run : runs) {
			if (run.keepMedia == SailSettings.KeepMedia.FAILED) {
				run.delete();
			}
		}
	}

	/**
	 * Ends the run: stops the screencast, and writes the film strip, the player and each directory's list of runs.
	 * Then, when the run passed and {@value SailSettings#KEEP_MEDIA} is {@code failed}, deletes the run's folders:
	 * at once, or when its unit ends (see {@link #beginUnit(String)}).
	 *
	 * <p>Side effects: writes, and may delete, files in the run's folders, and rewrites each directory's list of runs.
	 *
	 * @param automationPassed	whether the automation passed; a run with a failed step counts as failed even so
	 */
	void finish(boolean automationPassed) {
		if (screencast != null) {
			screencast.stop();
			screencast = null;
		}
		passed = automationPassed && steps.stream().noneMatch(StepRecord::failed);
		writePages();
		Unit unit = UNIT.get();
		if (unit != null) {
			unit.runs.add(this);
		}
		else if (passed && (keepMedia == SailSettings.KeepMedia.FAILED)) {
			delete();
		}
	}

	/** Writes the film strip, the player and each directory's list of runs. */
	private void writePages() {
		try {
			Path filmStrip = (snapshotRun == null) ? null : snapshotRun.resolve("index.html");
			Path player = (videoRun == null) ? null : videoRun.resolve("player.html");
			String summary = runJson(info, place, steps, passed);
			if (filmStrip != null) {
				String playerLink = (player == null) ? null : link(snapshotRun, player);
				Files.writeString(filmStrip, SailRunPages.filmStripHtml(info, steps, playerLink, link(snapshotRun, snapshotList.resolve("index.html"))),
									StandardCharsets.UTF_8);
				Files.writeString(snapshotRun.resolve(RUN_JSON), summary, StandardCharsets.UTF_8);
				writeRunsIndex(snapshotRun.getParent(), snapshotList);
			}
			if (player != null) {
				String filmStripLink = (filmStrip == null) ? null : link(videoRun, filmStrip);
				List<Frame> written;
				synchronized (frames) {
					written = new ArrayList<>(frames);
				}
				Files.writeString(videoRun.resolve("video.js"), SailRunPages.videoScript(info, written, steps, filmStripLink), StandardCharsets.UTF_8);
				Files.writeString(player, SailRunPages.playerHtml(info, steps, filmStripLink, link(videoRun, videoList.resolve("index.html"))),
									StandardCharsets.UTF_8);
				Files.writeString(videoRun.resolve(RUN_JSON), summary, StandardCharsets.UTF_8);
				writeRunsIndex(videoRun.getParent(), videoList);
			}
			selenide.trace("SAIL media written: " + steps.size() + " steps" +
							((videoRun == null) ? "" : ", " + frames.size() + " video frames") +
							((filmStrip == null) ? "" : "; film strip " + filmStrip) +
							((player == null) ? "" : "; player " + player));
		}
		catch (IOException | RuntimeException e) {
			selenide.trace("Could not write the SAIL media pages: " + e);
		}
	}

	/**
	 * Deletes the run's folders, and lists the runs left in each directory that had a list.
	 */
	private void delete() {
		Map<Path, Path> folders = new LinkedHashMap<>(4);
		if (snapshotRun != null) {
			folders.put(snapshotRun, snapshotList);
		}
		if (videoRun != null) {
			folders.putIfAbsent(videoRun, videoList);
		}
		for (Map.Entry<Path, Path> folder : folders.entrySet()) {
			Path run = folder.getKey();
			Path list = folder.getValue();
			try {
				org.skyve.util.FileUtil.delete(run.toFile());
				if (Files.exists(list.resolve("index.html"))) {
					writeRunsIndex(run.getParent(), list);
				}
			}
			catch (IOException | RuntimeException e) {
				selenide.trace("Could not delete the SAIL media of a passed run (" + SailSettings.KEEP_MEDIA + "=failed) in " + run + ": " + e);
				return;
			}
		}
		selenide.trace("SAIL media deleted, as the run passed (" + SailSettings.KEEP_MEDIA + "=failed): " + folders.keySet());
	}

	/**
	 * Rewrites a directory's list of runs, in its list folder (the directory, or for the default one the evidence folder
	 * above it; see {@link SailSettings#runsListDir(Path)}), and its runs' order for the run pages' previous and next run
	 * buttons ({@value #RUNS_ORDER_JS}), which stays in the directory beside the runs. A list moved up removes the one
	 * an earlier version left in the directory, so that there is one page to open. Synchronised: a load run's virtual
	 * users ({@link SailLoad}) end runs in one directory at the same time, and two writers of one file would garble it.
	 *
	 * @param directory	the directory of runs
	 * @param list	the folder for its list of runs
	 */
	private static synchronized void writeRunsIndex(@Nonnull Path directory, @Nonnull Path list) throws IOException {
		List<SailRunsList.ListedRun> runs = SailRunsList.listedRuns(directory);
		String runsLink = list.equals(directory) ? "" : link(list, directory) + '/';
		Files.writeString(list.resolve("index.html"), SailRunsList.runsIndexHtml(runs, runsLink), StandardCharsets.UTF_8);
		Files.writeString(directory.resolve(RUNS_ORDER_JS), SailRunsList.runsOrderScript(runs), StandardCharsets.UTF_8);
		if (! runsLink.isEmpty()) {
			Files.deleteIfExists(directory.resolve("index.html"));
		}
	}

	private static @Nonnull String link(@Nonnull Path from, @Nonnull Path to) {
		return from.relativize(to).toString().replace('\\', '/');
	}

	/**
	 * Renders a step as one line of SAIL XML, as the schema writes it; or, if it cannot be marshalled on its own, as the
	 * executor describes it. It is shown as it is (escaped), never parsed: the words come from the step itself.
	 */
	@Nonnull String stepXml(@Nonnull Step step) {
		String result = null;
		try {
			result = marshal(automation, step);
		}
		catch (RuntimeException e) {
			selenide.trace("Could not marshal a step for the SAIL media, so it is described instead: " + e);
		}
		return hidePasswords((result == null) ? InterpretedWebDriverExecutor.describe(step) : result);
	}

	/** What a hidden password shows as. */
	private static final String HIDDEN = "********";

	/**
	 * Hides a login's password in a step's XML or a failure that quotes it, and the value of a call's parameter whose
	 * name has "password" in it, unless it is a variable, so that the pages can be shown to others.
	 */
	static @Nonnull String hidePasswords(@Nonnull String stepXml) {
		return hideParameterPasswords(stepXml.replaceAll("(\\bpassword=\")(?!\\{var:)[^\"]*\"", "$1" + HIDDEN + '"'));
	}

	/** Hides the value of each {@code <parameter>} whose name has "password" in it, unless it is a variable. */
	private static @Nonnull String hideParameterPasswords(@Nonnull String stepXml) {
		StringBuilder result = new StringBuilder(stepXml.length());
		int from = 0;
		int tag = stepXml.indexOf("<parameter ");
		while (tag >= 0) {
			int tagEnd = stepXml.indexOf('>', tag);
			if (tagEnd < 0) {
				break;
			}
			String element = stepXml.substring(tag, tagEnd);
			String name = attribute(element, "name");
			String value = attribute(element, "value");
			if ((name != null) && (value != null) && (! hidden(name, value).equals(value))) {
				int valueStart = tag + element.indexOf(" value=\"") + 8;
				result.append(stepXml, from, valueStart).append(HIDDEN);
				from = valueStart + value.length();
			}
			tag = stepXml.indexOf("<parameter ", tagEnd);
		}
		return result.append(stepXml, from, stepXml.length()).toString();
	}

	/** Returns an element's attribute as written in its start tag, or null if it has none. */
	private static @Nullable String attribute(@Nonnull String element, @Nonnull String name) {
		String start = ' ' + name + "=\"";
		int valueStart = element.indexOf(start);
		if (valueStart < 0) {
			return null;
		}
		valueStart += start.length();
		int valueEnd = element.indexOf('"', valueStart);
		return (valueEnd < 0) ? null : element.substring(valueStart, valueEnd);
	}

	/** Returns a named value, hidden if its name has "password" in it and it is not a variable. */
	static @Nonnull String hidden(@Nonnull String name, @Nonnull String value) {
		return (name.toLowerCase(Locale.ROOT).contains("password") && (! value.startsWith("{var:"))) ? HIDDEN : value;
	}

	/**
	 * Marshals a step as the schema writes it, on one line, without namespaces.
	 *
	 * @param automation	the automation the step is in, whose UX/UI and user agent the marshalled wrapper needs
	 * @param step	the step
	 * @return the step's XML, or null if it is not in the marshalled text
	 * @throws RuntimeException	if the step cannot be marshalled on its own
	 */
	static @Nullable String marshal(@Nonnull Automation automation, @Nonnull Step step) {
		Automation wrapper = new Automation();
		wrapper.setUxui(automation.getUxui());
		wrapper.setUserAgentType(automation.getUserAgentType());
		Interaction interaction = new Interaction();
		interaction.setName("step");
		interaction.getSteps().add(step);
		wrapper.getInteractions().add(interaction);
		Matcher matcher = METHOD_CONTENT.matcher(XMLMetaData.marshalSAIL(wrapper));
		if (! matcher.find()) {
			return null;
		}
		return matcher.group(1)
				// one white space before each namespace declaration: a \s+ here would backtrack over long runs of spaces
				.replaceAll("\\sxmlns(?::\\w+)?=\"[^\"]*\"", "")
				.replaceAll("<(/?)\\w+:", "<$1")
				.replaceAll("\\s+", " ")
				.trim();
	}

	/** Makes text safe for a file name: letters, digits and dashes, at most {@code max} characters. */
	static @Nonnull String sanitise(@Nonnull String text, int max) {
		String result = text.replaceAll("[^A-Za-z0-9.]+", "-").replaceAll("-{2,}", "-").replaceAll("^-|-$", "");
		if (result.length() > max) {
			result = result.substring(0, max).replaceAll("-$", "");
		}
		return result.isEmpty() ? "step" : result;
	}

	/** How long the steps took, from the first's start to the last's end. */
	static long runMillis(@Nonnull List<StepRecord> steps) {
		return steps.isEmpty() ? 0L : Math.max(0L, steps.get(steps.size() - 1).endMillis() - steps.get(0).startMillis());
	}

	/**
	 * Writes a run's summary for the list of runs ({@value #RUN_JSON} in each of its folders).
	 *
	 * @param info	the run
	 * @param place	where it is in its session
	 * @param steps	the executed steps
	 * @param passed	whether the run passed
	 * @return the JSON
	 */
	static @Nonnull String runJson(@Nonnull RunInfo info, @Nonnull Place place, @Nonnull List<StepRecord> steps, boolean passed) {
		Map<String, Object> run = new LinkedHashMap<>();
		run.put("session", place.session());
		run.put("unit", Integer.valueOf(place.unit()));
		run.put("leg", Integer.valueOf(place.leg()));
		run.put("title", info.title());
		run.put("test", info.test());
		run.put("label", info.label());
		run.put("started", info.started());
		run.put("uxui", info.uxui());
		run.put("passed", Boolean.valueOf(passed));
		run.put("steps", Integer.valueOf(steps.size()));
		run.put("failed", Long.valueOf(steps.stream().filter(StepRecord::failed).count()));
		run.put("millis", Long.valueOf(runMillis(steps)));
		Story story = info.story();
		run.put("suite", story.suite());
		run.put("suiteDescription", story.suiteDescription());
		run.put("workflowDescription", story.workflowDescription());
		run.put("description", story.description());
		return new Json().toJson(run);
	}

	/**
	 * Chrome's DevTools screencast of one browser window: each frame is written to {@code frames/} with the time Chrome
	 * drew it. The browser's control ({@link SailBrowserControl}, capability {@link Capability#SCREENCAST}) records it
	 * and acknowledges each frame so that Chrome sends the next; this writes them.
	 * <p>
	 * It opens no DevTools connection or session of its own, on purpose: a second session attached to a page makes
	 * any document loaded after the browser goes offline report {@code navigator.onLine === true}, though its
	 * requests fail, so video would change what a test sees. The control's own session for the window carries the
	 * screencast instead (see {@link PageScreencast}). A browser whose control lacks the capability (not Chromium) has
	 * no video.
	 */
	static final class Screencast {
		private final PageScreencast recorder;
		private final Path folder;
		private final List<Frame> frames;
		private final Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide;
		/** Writes frames in order, off the test thread and off the control's DevTools thread. */
		private final ExecutorService worker = Executors.newSingleThreadExecutor(r -> {
			Thread result = new Thread(r, "sail-screencast");
			result.setDaemon(true);
			return result;
		});

		private Screencast(@Nonnull PageScreencast recorder,
							@Nonnull Path folder,
							@Nonnull List<Frame> frames,
							@Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide) {
			this.recorder = recorder;
			this.folder = folder;
			this.frames = frames;
			this.selenide = selenide;
		}

		/**
		 * Starts the screencast of a browser's current window.
		 *
		 * @param driver	the browser
		 * @param folder	the run's video folder
		 * @param frames	the run's frames, which this adds to (guarded by itself)
		 * @param selenide	for the trace
		 * @return the screencast, or null when the browser has no screencast (not Chromium) or it cannot start
		 */
		static @Nullable Screencast start(@Nonnull WebDriver driver,
											@Nonnull Path folder,
											@Nonnull List<Frame> frames,
											@Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide) {
			try {
				// For Chromium this makes the control, which connects to DevTools and attaches to the page
				SailBrowserControl control = SailBrowserControl.of(driver);
				return start(control, driver.getWindowHandle(), folder, frames, selenide);
			}
			catch (RuntimeException e) {
				selenide.trace("SAIL video could not start, so this run has none: " + e);
				return null;
			}
		}

		/**
		 * Starts the screencast of a window of a browser that has one.
		 *
		 * @param control	the browser's control
		 * @param windowHandle	the window
		 * @param folder	the run's video folder
		 * @param frames	the run's frames, which this adds to (guarded by itself)
		 * @param selenide	for the trace
		 * @return the screencast, or null when the control has no screencast or it cannot start
		 */
		static @Nullable Screencast start(@Nonnull SailBrowserControl control,
											@Nonnull String windowHandle,
											@Nonnull Path folder,
											@Nonnull List<Frame> frames,
											@Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide) {
			if (! control.supports(Capability.SCREENCAST)) {
				selenide.trace("SAIL video is not supported for " + control.browser() +
								" (it needs Chromium's DevTools); carrying on without it");
				return null;
			}
			PageScreencast recorder = control.require(PageScreencast.class, "SAIL video");
			Screencast result = new Screencast(recorder, folder, frames, selenide);
			try {
				recorder.startScreencast(windowHandle,
											new PageScreencast.Options(FRAME_FORMAT, FRAME_QUALITY, FRAME_MAX_WIDTH, FRAME_MAX_HEIGHT),
											result::received);
			}
			catch (RuntimeException e) {
				selenide.trace("SAIL video could not start, so this run has none: " + e);
				result.worker.shutdown();
				return null;
			}
			selenide.trace(String.format("SAIL video: Chrome DevTools screencast on the browser control's own page session, %s quality %d, at most %dx%d",
											FRAME_FORMAT, Integer.valueOf(FRAME_QUALITY), Integer.valueOf(FRAME_MAX_WIDTH), Integer.valueOf(FRAME_MAX_HEIGHT)));
			return result;
		}

		/**
		 * Called on the control's DevTools thread for each frame, which the control has acknowledged already: hands it
		 * to the worker to write.
		 */
		private void received(@Nonnull PageScreencast.Image frame) {
			try {
				worker.execute(() -> write(frame));
			}
			catch (@SuppressWarnings("unused") RejectedExecutionException e) {
				// the screencast has stopped; a frame already on its way is dropped
			}
		}

		private void write(@Nonnull PageScreencast.Image frame) {
			try {
				byte[] image = Base64.getDecoder().decode(frame.base64());
				String file;
				synchronized (frames) {
					file = String.format("frames/%06d.jpg", Integer.valueOf(frames.size() + 1));
					frames.add(new Frame(file, frame.millis()));
				}
				Files.write(folder.resolve(file), image);
			}
			catch (IOException | RuntimeException e) {
				selenide.trace("Could not save a SAIL video frame: " + e);
			}
		}

		/**
		 * Stops the screencast and waits for the frames already received to be written.
		 */
		void stop() {
			try {
				recorder.stopScreencast();
			}
			catch (RuntimeException e) { // the browser has gone
				selenide.trace("Could not stop the SAIL video screencast: " + e);
			}
			worker.shutdown();
			try {
				// Wait for the worker to write the frames it has (an event on another thread; bounded)
				if (! worker.awaitTermination(selenide.timeout().toMillis(), TimeUnit.MILLISECONDS)) {
					selenide.trace("SAIL video frames were still being written when the run ended");
				}
			}
			catch (@SuppressWarnings("unused") InterruptedException e) {
				Thread.currentThread().interrupt();
			}
		}
	}
}
