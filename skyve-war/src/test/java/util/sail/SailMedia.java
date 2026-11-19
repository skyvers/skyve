package util.sail;

import java.io.IOException;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.devtools.Command;
import org.openqa.selenium.devtools.Connection;
import org.openqa.selenium.devtools.Event;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.SeleniumCdpConnection;
import org.openqa.selenium.devtools.idealized.target.model.SessionID;
import org.openqa.selenium.json.Json;
import org.openqa.selenium.json.JsonInput;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Step;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Step snapshots and a video of one automation run, for a person to look through what a SAIL run did.
 * <p>
 * Both are opt-in, by {@link SailSettings} (a directory, or an evidence request); without them {@link #start} returns
 * null and the executor does nothing more.
 * <ul>
 * <li>{@value SailSettings#SNAPSHOT_DIR}: after each step (and when a step fails, before the test strategy acts on
 * it), a screenshot named by sequence, interaction and step, and an {@code index.html} film strip of the steps, under
 * their interactions: each step in words ({@link #summary(String)}) and as SAIL, its outcome and duration, and its
 * screenshot. With {@value SailSettings#SNAPSHOT_SOURCE}{@code =true}, also each page's source.</li>
 * <li>{@value SailSettings#VIDEO_DIR}: the browser's frames, from Chrome's DevTools screencast, with their times,
 * and a {@code player.html} that plays them at their real timing, fitted to the window, captioned with the step that
 * was running, beside a list of the steps in words to jump to.</li>
 * </ul>
 * Each automation run gets its own folder under each directory, named by its time, test and (for a suite file) the
 * script, so suites, workflows and ITs with several tests keep every run, with a {@value #RUN_JSON} summary of it.
 * Each directory gets an {@code index.html} listing its runs by what they do (their first interaction), with their
 * outcomes, and filters. Everything is static HTML that opens from the file system, light or dark as the system is.
 * The run's pages have a Settings button that shows its settings that do not come from their defaults, with their
 * sources. With {@value SailSettings#KEEP_MEDIA}{@code =failed}, a run that passed deletes its folders when it ends.
 * <p>
 * Recordings show what the browser showed: a run against real data records real data.
 * <p>
 * Threading: an instance belongs to the thread running its automation. Screencast frames arrive on the CDP
 * connection's thread, which acknowledges each without waiting and hands it to a single worker thread to write, so the
 * test thread never waits for them until the run ends.
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
	/** The longest a value or script is shown in a step's summary. */
	private static final int SUMMARY_VALUE_MAX = 140;
	/** The script a suite is running on this thread, to name the run's folder; see {@link #label(String)}. */
	private static final ThreadLocal<String> LABEL = new ThreadLocal<>();
	/**
	 * The runs of the unit (a workflow) running on this thread, whose media is kept or deleted together; null outside
	 * a unit. Set by {@link #beginUnit()} and removed by {@link #endUnit(boolean)}, which its caller calls in a
	 * {@code finally}.
	 */
	private static final ThreadLocal<List<SailMedia>> UNIT = new ThreadLocal<>();
	/**
	 * This JVM's run of tests, for the list of runs to show each session's runs together in the order they ran: when
	 * this class was loaded, as a folder time.
	 */
	static final String SESSION = FOLDER_TIME.format(LocalDateTime.now());
	/** Numbers the units (see {@link #beginUnit()}) of this session. */
	private static final AtomicInteger UNITS = new AtomicInteger();
	/** The unit running on this thread: its number and how many runs it has started; null outside a unit. */
	private static final ThreadLocal<int[]> UNIT_PLACE = new ThreadLocal<>();

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

	/** One executed step: where it ran, what it was, when, how it went, and its snapshot. */
	record StepRecord(int sequence,
						@Nonnull String procedure,
						int number,
						@Nonnull String xml,
						long startMillis,
						long endMillis,
						@Nullable String failure,
						@Nullable String screenshot,
						@Nullable String source) {
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
	 */
	record RunInfo(@Nonnull String name,
					@Nonnull String title,
					@Nonnull String test,
					@Nullable String label,
					@Nonnull String started,
					@Nonnull String uxui,
					@Nonnull List<String> settings) {
		// a value
	}

	private final Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide;
	private final Automation automation;
	private final RunInfo info;
	private final Place place;
	private final Path snapshotRun;
	private final Path videoRun;
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
	private int number;
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
		int[] unit = UNIT_PLACE.get();
		this.place = (unit == null) ? new Place(SESSION, 0, 0) : new Place(SESSION, unit[0], ++unit[1]);
		this.snapshotRun = snapshotRun;
		this.videoRun = videoRun;
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
		RunInfo info = new RunInfo(name, title(automation, test), test, label, SHOWN_TIME.format(now), automation.getUxui(), settings.describe());
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
	 * @param stepNumber	the step's number in it, from 1
	 * @param step	the step
	 */
	void stepStarted(@Nonnull String procedureName, int stepNumber, @Nonnull Step step) {
		sequence++;
		procedure = procedureName;
		number = stepNumber;
		xml = stepXml(step);
		fileStem = String.format("%03d-%s", Integer.valueOf(sequence), sanitise(procedureName + '-' + opCode(xml), 80));
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
		steps.add(new StepRecord(sequence, procedure, number, xml, startMillis, endMillis,
									(failure == null) ? null : hidePasswords(failure), screenshot, source));
		xml = null;
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
	 * passed (see {@link #endUnit(boolean)}). Outside a unit, each run decides for itself.
	 *
	 * <p>Side effects: sets this thread's unit, which the caller must end in a {@code finally}.
	 */
	static void beginUnit() {
		UNIT.set(new ArrayList<>());
		UNIT_PLACE.set(new int[] {UNITS.incrementAndGet(), 0});
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
		List<SailMedia> runs = UNIT.get();
		UNIT.remove();
		UNIT_PLACE.remove();
		if ((runs == null) || (! unitPassed)) {
			return;
		}
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
	 * at once, or when its unit ends (see {@link #beginUnit()}).
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
		List<SailMedia> unit = UNIT.get();
		if (unit != null) {
			unit.add(this);
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
				Files.writeString(filmStrip, filmStripHtml(info, steps, playerLink), StandardCharsets.UTF_8);
				Files.writeString(snapshotRun.resolve(RUN_JSON), summary, StandardCharsets.UTF_8);
				writeRunsIndex(snapshotRun.getParent());
			}
			if (player != null) {
				String filmStripLink = (filmStrip == null) ? null : link(videoRun, filmStrip);
				List<Frame> written;
				synchronized (frames) {
					written = new ArrayList<>(frames);
				}
				Files.writeString(videoRun.resolve("video.js"), videoScript(info, written, steps, filmStripLink), StandardCharsets.UTF_8);
				Files.writeString(player, playerHtml(info, steps, filmStripLink), StandardCharsets.UTF_8);
				Files.writeString(videoRun.resolve(RUN_JSON), summary, StandardCharsets.UTF_8);
				writeRunsIndex(videoRun.getParent());
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
		List<Path> folders = new ArrayList<>(2);
		if (snapshotRun != null) {
			folders.add(snapshotRun);
		}
		if ((videoRun != null) && (! videoRun.equals(snapshotRun))) {
			folders.add(videoRun);
		}
		for (Path folder : folders) {
			try {
				org.skyve.util.FileUtil.delete(folder.toFile());
				if (Files.exists(folder.getParent().resolve("index.html"))) {
					writeRunsIndex(folder.getParent());
				}
			}
			catch (IOException | RuntimeException e) {
				selenide.trace("Could not delete the SAIL media of a passed run (" + SailSettings.KEEP_MEDIA + "=failed) in " + folder + ": " + e);
				return;
			}
		}
		selenide.trace("SAIL media deleted, as the run passed (" + SailSettings.KEEP_MEDIA + "=failed): " + folders);
	}

	/**
	 * Rewrites a directory's list of runs. Synchronised: a load run's virtual users ({@link SailLoad}) end runs in one
	 * directory at the same time, and two writers of one file would garble it.
	 */
	private static synchronized void writeRunsIndex(@Nonnull Path directory) throws IOException {
		Files.writeString(directory.resolve("index.html"), runsIndexHtml(directory), StandardCharsets.UTF_8);
	}

	private static @Nonnull String link(@Nonnull Path from, @Nonnull Path to) {
		return from.relativize(to).toString().replace('\\', '/');
	}

	/**
	 * Renders a step as one line of SAIL XML, as the schema writes it; or, if it cannot be marshalled on its own, as the
	 * executor describes it.
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

	/**
	 * Hides a login's password in a step's XML or a failure that quotes it, unless it is a variable, so that the pages
	 * can be shown to others.
	 */
	static @Nonnull String hidePasswords(@Nonnull String stepXml) {
		return stepXml.replaceAll("(\\bpassword=\")(?!\\{var:)[^\"]*\"", "$1********\"");
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

	/** Returns the op-code of a step's XML, e.g. {@code dataEnter}. */
	static @Nonnull String opCode(@Nonnull String stepXml) {
		Matcher matcher = Pattern.compile("^<\\s*([\\w]+)").matcher(stepXml);
		return matcher.find() ? matcher.group(1) : "step";
	}

	/** Makes text safe for a file name: letters, digits and dashes, at most {@code max} characters. */
	static @Nonnull String sanitise(@Nonnull String text, int max) {
		String result = text.replaceAll("[^A-Za-z0-9.]+", "-").replaceAll("-{2,}", "-").replaceAll("^-|-$", "");
		if (result.length() > max) {
			result = result.substring(0, max).replaceAll("-$", "");
		}
		return result.isEmpty() ? "step" : result;
	}

	static @Nonnull String escape(@Nullable String text) {
		if (text == null) {
			return "";
		}
		StringBuilder result = new StringBuilder(text.length() + 16);
		for (int i = 0, l = text.length(); i < l; i++) {
			char c = text.charAt(i);
			switch (c) {
				case '<' -> result.append("&lt;");
				case '>' -> result.append("&gt;");
				case '&' -> result.append("&amp;");
				case '"' -> result.append("&quot;");
				case '\'' -> result.append("&#39;");
				default -> result.append(c);
			}
		}
		return result.toString();
	}

	/**
	 * Returns a step in plain words, for the step lists: what it does, without the SAIL markup. A comment is its text,
	 * a test script its message, a call its class's name in words with its parameters, a script or a wait its code
	 * (shortened), a login who signs in, and any other step its op-code in words with its attributes.
	 *
	 * @param stepXml	the step's XML, as {@link #stepXml(Step)} renders it
	 * @return the summary
	 */
	static @Nonnull String summary(@Nonnull String stepXml) {
		StepParts step = StepParts.parse(stepXml);
		Map<String, String> attributes = step.attributes();
		String variable = attributes.get("variable");
		String into = (variable == null) ? "" : " (into " + variable + ")";
		return switch (step.op()) {
			case "comment" -> shorten(step.text(), 400);
			case "testScript" -> "Check: " + (attributes.containsKey("message") ? attributes.get("message") : script(step.code()));
			case "waitFor" -> "Wait for: " + script(step.code());
			case "execute", "executeAsync" -> "Run: " + script(step.code()) + into;
			case "call" -> words(simpleName(attributes.getOrDefault("className", "call"))) + pairs(step.parameters()) + into;
			case "login" -> "Sign in as " + attributes.getOrDefault("user", "?") + ("true".equals(attributes.get("remember")) ? ", remembered" : "");
			case "logout" -> "Sign out";
			case "navigateLink" -> "Go to " + attributes.getOrDefault("href", "?");
			case "pause" -> "Pause " + attributes.getOrDefault("millis", "?") + " ms";
			default -> {
				Map<String, String> shown = new LinkedHashMap<>(attributes);
				shown.remove("timeoutMillis");
				yield words(step.op()) + pairs(shown);
			}
		};
	}

	/**
	 * A step's parts, for {@link #summary(String)}: its op-code, attributes, text and parameters.
	 *
	 * @param op	the op-code, e.g. {@code waitFor}
	 * @param attributes	the attributes, in order
	 * @param text	the text inside it (outside its child elements), trimmed
	 * @param parameters	a call's parameters, by name, in order
	 */
	record StepParts(@Nonnull String op, @Nonnull Map<String, String> attributes, @Nonnull String text, @Nonnull Map<String, String> parameters) {
		/** The script a scripted step runs: its {@code script} attribute (an executor's description) or its text. */
		@Nonnull String code() {
			String script = attributes.get("script");
			return (script == null) ? text : script;
		}

		/**
		 * Parses a step's XML; or, when it is not well-formed (an executor's description, which does not escape its
		 * values), takes its op-code and attributes as they are written.
		 */
		static @Nonnull StepParts parse(@Nonnull String stepXml) {
			try {
				DocumentBuilder builder;
				synchronized (XML) { // a factory is not thread-safe, and a load run's users write pages at once
					builder = XML.newDocumentBuilder();
				}
				builder.setErrorHandler(new DefaultHandler()); // fails on a fatal error, without printing it
				Element root = builder.parse(new InputSource(new StringReader(stepXml))).getDocumentElement();
				Map<String, String> attributes = new LinkedHashMap<>();
				NamedNodeMap nodes = root.getAttributes();
				for (int i = 0, l = nodes.getLength(); i < l; i++) {
					Node node = nodes.item(i);
					attributes.put(node.getNodeName(), node.getNodeValue());
				}
				StringBuilder text = new StringBuilder();
				Map<String, String> parameters = new LinkedHashMap<>();
				for (Node child = root.getFirstChild(); child != null; child = child.getNextSibling()) {
					if ((child.getNodeType() == Node.TEXT_NODE) || (child.getNodeType() == Node.CDATA_SECTION_NODE)) {
						text.append(child.getNodeValue());
					}
					else if ((child instanceof Element parameter) && "parameter".equals(parameter.getTagName())) {
						parameters.put(parameter.getAttribute("name"), parameter.getAttribute("value"));
					}
				}
				return new StepParts(root.getTagName(), attributes, text.toString().trim(), parameters);
			}
			catch (@SuppressWarnings("unused") ParserConfigurationException | SAXException | IOException e) {
				return new StepParts(opCode(stepXml), describedAttributes(stepXml), "", Map.of());
			}
		}
	}

	/**
	 * Returns the {@code name="value"} attributes of an executor's description of a step, which does not escape its
	 * values (so it is not XML); a value runs to the next double quote. A description is one element, and its script may
	 * hold {@code >}, so the whole text is read.
	 */
	static @Nonnull Map<String, String> describedAttributes(@Nonnull String description) {
		Map<String, String> result = new LinkedHashMap<>();
		int equals = description.indexOf("=\"");
		while (equals >= 0) {
			int nameStart = equals;
			while ((nameStart > 0) && (Character.isLetterOrDigit(description.charAt(nameStart - 1)) || (description.charAt(nameStart - 1) == '_'))) {
				nameStart--;
			}
			int valueEnd = description.indexOf('"', equals + 2);
			if (valueEnd < 0) {
				break;
			}
			if (nameStart < equals) {
				result.put(description.substring(nameStart, equals), description.substring(equals + 2, valueEnd));
			}
			equals = description.indexOf("=\"", valueEnd + 1);
		}
		return result;
	}

	/** Parses steps for {@link StepParts}; no DTDs or external entities (a step is never a document of its own). */
	private static final DocumentBuilderFactory XML = xmlFactory();

	private static @Nonnull DocumentBuilderFactory xmlFactory() {
		try {
			DocumentBuilderFactory result = DocumentBuilderFactory.newInstance();
			result.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
			result.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
			result.setXIncludeAware(false);
			result.setExpandEntityReferences(false);
			return result;
		}
		catch (ParserConfigurationException e) {
			throw new IllegalStateException("No safe XML parser for the SAIL media pages", e);
		}
	}

	/** A script as one shortened line, without a leading {@code return} or a trailing semicolon. */
	static @Nonnull String script(@Nonnull String code) {
		String result = code.replaceAll("\\s+", " ").trim();
		if (result.startsWith("return ")) {
			result = result.substring(7);
		}
		if (result.endsWith(";")) {
			result = result.substring(0, result.length() - 1).trim();
		}
		return shorten(result, SUMMARY_VALUE_MAX);
	}

	/** Shortens text to at most {@code max} characters, ending in an ellipsis when it was cut. */
	static @Nonnull String shorten(@Nonnull String text, int max) {
		String result = text.replaceAll("\\s+", " ").trim();
		return (result.length() <= max) ? result : result.substring(0, max - 1).trim() + '…';
	}

	/** Names as {@code (name: value, ...)}, each value shortened; empty when there are none. */
	private static @Nonnull String pairs(@Nonnull Map<String, String> values) {
		if (values.isEmpty()) {
			return "";
		}
		StringBuilder result = new StringBuilder(" (");
		values.forEach((name, value) -> {
			if (result.length() > 2) {
				result.append(", ");
			}
			result.append(name).append(": ").append(shorten(value, SUMMARY_VALUE_MAX));
		});
		return result.append(')').toString();
	}

	/** A class's name without its package (and outer classes). */
	private static @Nonnull String simpleName(@Nonnull String className) {
		return className.substring(Math.max(className.lastIndexOf('.'), className.lastIndexOf('$')) + 1);
	}

	/** A camel-case name in words, e.g. {@code ReplaceFrontElevation} as "Replace front elevation"; acronyms stay. */
	static @Nonnull String words(@Nonnull String name) {
		// a space where a word starts: after a lower case letter or digit, or at the last capital of an acronym
		String[] parts = name.replaceAll("(?<=[a-z0-9])(?=[A-Z])", " ").replaceAll("(?<=[A-Z])(?=[A-Z][a-z])", " ").trim().split(" ");
		StringBuilder result = new StringBuilder(name.length() + 8);
		for (int i = 0; i < parts.length; i++) {
			String part = parts[i];
			if (part.isEmpty()) {
				continue;
			}
			if (i == 0) {
				result.append(Character.toUpperCase(part.charAt(0))).append(part.substring(1));
			}
			else {
				boolean acronym = (part.length() > 1) && part.equals(part.toUpperCase(Locale.ROOT));
				result.append(' ').append(acronym ? part : part.toLowerCase(Locale.ROOT));
			}
		}
		return result.toString();
	}

	/** A duration as shown: {@code 250 ms}, {@code 1.3 s} or {@code 2 min 5 s}. */
	static @Nonnull String duration(long millis) {
		if (millis < 1000L) {
			return millis + " ms";
		}
		if (millis < 60_000L) {
			return String.format(Locale.ROOT, "%.1f s", Double.valueOf(millis / 1000.0));
		}
		long seconds = Math.round(millis / 1000.0);
		return (seconds / 60L) + " min " + (seconds % 60L) + " s";
	}

	/** How long the steps took, from the first's start to the last's end. */
	private static long runMillis(@Nonnull List<StepRecord> steps) {
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
		return new Json().toJson(run);
	}

	/**
	 * The pages' shared style: light and dark from the system setting, a header with the run's title and actions, cards,
	 * tables, badges and the settings pop-up.
	 */
	private static final String STYLE = """
			<style>
			:root{--bg:#f5f6f8;--surface:#fff;--sunken:#eceff3;--text:#1f2328;--muted:#5f6873;--border:#d9dee4;
			--accent:#0b62c4;--accent-soft:#e2edfb;--pass:#1a7f37;--pass-soft:#dcf5e3;--fail:#c42b1c;--fail-soft:#fdecea;
			--shadow:0 1px 2px rgba(16,24,40,.05),0 1px 3px rgba(16,24,40,.08);color-scheme:light dark}
			@media (prefers-color-scheme:dark){:root{--bg:#0f1216;--surface:#171b21;--sunken:#0b0e12;--text:#e5e8ec;--muted:#98a2ad;
			--border:#2a3038;--accent:#78b2ff;--accent-soft:#1b2c44;--pass:#4cc36f;--pass-soft:#123020;--fail:#ff7b72;--fail-soft:#3a1c1c;
			--shadow:none}}
			*{box-sizing:border-box}
			body{margin:0;background:var(--bg);color:var(--text);font:14px/1.45 -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,sans-serif}
			a{color:var(--accent);text-decoration:none}a:hover{text-decoration:underline}
			.top{display:flex;flex-wrap:wrap;gap:10px 20px;align-items:flex-start;justify-content:space-between;padding:14px 20px;
			background:var(--surface);border-bottom:1px solid var(--border)}
			.heading{min-width:0;flex:1 1 300px}
			.top h1{font-size:18px;line-height:1.3;margin:0 0 6px;font-weight:600;overflow-wrap:anywhere}
			.sub{display:flex;flex-wrap:wrap;gap:4px 14px;align-items:center;color:var(--muted);font-size:13px}
			.nav{display:flex;flex-wrap:wrap;gap:8px;align-items:center}
			.btn{display:inline-flex;align-items:center;gap:6px;font:inherit;font-size:13px;line-height:1.2;padding:6px 12px;
			border:1px solid var(--border);border-radius:6px;background:var(--surface);color:var(--text);cursor:pointer;text-decoration:none}
			.btn:hover{background:var(--sunken);text-decoration:none}
			.btn.primary{background:var(--accent);border-color:var(--accent);color:#fff;min-width:72px;justify-content:center}
			@media (prefers-color-scheme:dark){.btn.primary{color:#0b0e12}}
			.badge{display:inline-block;padding:1px 9px;border-radius:999px;font-size:12px;font-weight:600;line-height:1.6;
			background:var(--sunken);color:var(--muted);white-space:nowrap}
			.badge.pass{background:var(--pass-soft);color:var(--pass)}.badge.fail{background:var(--fail-soft);color:var(--fail)}
			.content{padding:16px 20px 32px}
			.card{background:var(--surface);border:1px solid var(--border);border-radius:8px;box-shadow:var(--shadow)}
			table{border-collapse:separate;border-spacing:0;width:100%}
			th,td{padding:9px 12px;text-align:left;vertical-align:top;border-bottom:1px solid var(--border)}
			tbody tr:last-child td{border-bottom:0}
			thead th{position:sticky;top:0;z-index:1;background:var(--sunken);color:var(--muted);font-size:11px;font-weight:600;
			text-transform:uppercase;letter-spacing:.04em}
			thead th:first-child{border-top-left-radius:8px}thead th:last-child{border-top-right-radius:8px}
			tr.fail td{background:var(--fail-soft)}tr.fail .badge.fail{background:var(--surface)}
			tr.group td{background:var(--bg);font-weight:600;font-size:13px;padding-top:14px}
			.summary{font-weight:500;overflow-wrap:anywhere}
			code,.xml{font:12px/1.45 ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;white-space:pre-wrap;overflow-wrap:anywhere}
			.xml{display:block;color:var(--muted);margin-top:4px}
			.failure{color:var(--fail);white-space:pre-wrap;overflow-wrap:anywhere;margin-top:4px;font-size:13px}
			.muted{color:var(--muted)}.small{font-size:12px}
			.num{white-space:nowrap;font-variant-numeric:tabular-nums}
			td.links{white-space:nowrap}td.links .btn+.btn{margin-left:6px}
			.session{margin-bottom:28px}
			.session h2{display:flex;flex-wrap:wrap;gap:6px 10px;align-items:center;font-size:15px;font-weight:600;margin:4px 0 10px}
			.session h2 .muted{font-weight:400;font-size:13px}
			tr.unit td{border-bottom-style:dashed}
			tr.leg td:nth-child(3){padding-left:30px;position:relative}
			tr.leg td:nth-child(3)::before{content:"";position:absolute;left:14px;top:0;bottom:0;border-left:2px solid var(--border)}
			.leg-no{float:right;margin-left:8px;font-size:11px;color:var(--muted);text-transform:uppercase;letter-spacing:.04em}
			td.shot img{display:block;max-width:300px;border:1px solid var(--border);border-radius:4px}
			.pop{width:min(760px,calc(100vw - 32px));max-height:calc(100vh - 64px);overflow:auto;padding:18px 20px;
			border:1px solid var(--border);border-radius:10px;background:var(--surface);color:var(--text);box-shadow:0 12px 32px rgba(0,0,0,.25)}
			.pop::backdrop{background:rgba(0,0,0,.3)}
			.pop-head{display:flex;justify-content:space-between;align-items:center;gap:12px;margin-bottom:12px}
			.pop h2{font-size:16px;margin:0}.pop h3{font-size:12px;text-transform:uppercase;letter-spacing:.04em;color:var(--muted);margin:16px 0 6px}
			.pop dl{display:grid;grid-template-columns:max-content minmax(0,1fr);gap:6px 16px;margin:0}
			.pop dt{color:var(--muted)}.pop dd{margin:0;overflow-wrap:anywhere}.pop dd .muted{font-size:12px}
			@media (max-width:700px){.top{padding:12px 16px}.heading{flex-basis:100%}.content{padding:12px 16px 24px}.table-wrap{overflow-x:auto}
			td.shot img{max-width:160px}.pop dl{grid-template-columns:1fr}.pop dd{margin-bottom:6px}}
			</style>
			""";

	/** The player's own style: the toolbar, the frame fitted to the window, its caption, and the step list. */
	private static final String PLAYER_STYLE = """
			<style>
			body.player-page{height:100vh;display:flex;flex-direction:column;overflow:hidden}
			.toolbar{display:flex;flex-wrap:wrap;gap:8px;align-items:center;padding:10px 20px;background:var(--surface);border-bottom:1px solid var(--border)}
			.toolbar label{color:var(--muted);font-size:13px;display:inline-flex;align-items:center;gap:6px}
			.toolbar select{font:inherit;font-size:13px;padding:4px 6px;border:1px solid var(--border);border-radius:6px;background:var(--surface);color:var(--text)}
			#seek{flex:1;min-width:160px;accent-color:var(--accent)}
			#clock{color:var(--muted);font-size:13px;min-width:96px;text-align:right}
			.stage{flex:1;min-height:0;display:grid;grid-template-columns:minmax(0,1fr) minmax(280px,400px);gap:16px;padding:16px 20px}
			.screen{display:flex;flex-direction:column;gap:12px;min-height:0;min-width:0}
			.frame-box{position:relative;flex:1;min-height:200px;background:var(--sunken);border:1px solid var(--border);border-radius:8px;overflow:hidden}
			#frame{position:absolute;inset:0;width:100%;height:100%;object-fit:contain}
			.caption{flex:none;max-height:36vh;overflow:auto;padding:12px 16px}
			.caption.fail{border-color:var(--fail)}
			.cap-meta{display:flex;flex-wrap:wrap;gap:8px;align-items:center;color:var(--muted);font-size:12px;margin-bottom:6px}
			.cap-group{color:var(--muted);font-size:13px;overflow-wrap:anywhere}
			.cap-summary{font-size:15px;font-weight:600;margin:2px 0 4px;overflow-wrap:anywhere}
			.steps-panel{display:flex;flex-direction:column;min-height:0;overflow:hidden}
			.panel-head{padding:10px 14px;border-bottom:1px solid var(--border);font-weight:600;display:flex;justify-content:space-between;gap:8px}
			.panel-head .muted{font-weight:400;font-size:12px}
			#steps{flex:1;min-height:0;overflow:auto;padding-bottom:8px}
			#steps h3{position:sticky;top:0;z-index:1;margin:0;padding:10px 14px 4px;background:var(--surface);color:var(--muted);font-size:12px;font-weight:600;overflow-wrap:anywhere}
			#steps ol{margin:0;padding:0 8px 0 42px}
			#steps li{padding:4px 8px;border-radius:6px;cursor:pointer;font-size:13px}
			#steps li::marker{color:var(--muted);font-size:11px;font-variant-numeric:tabular-nums}
			#steps li:hover{background:var(--sunken)}
			#steps li.current{background:var(--accent-soft)}
			#steps li.fail .summary{color:var(--fail)}
			#steps li .failure{display:-webkit-box;-webkit-line-clamp:2;-webkit-box-orient:vertical;overflow:hidden;font-size:12px;margin-top:2px}
			@media (max-width:900px){body.player-page{height:auto;overflow:visible}.stage{grid-template-columns:minmax(0,1fr);padding:12px 16px}
			.frame-box{height:70vh}.caption{max-height:none}.steps-panel{max-height:60vh}.toolbar{padding:10px 16px}}
			</style>
			""";

	/** The player's script: plays {@code video.js}'s frames at their real timing, captioned with the step. */
	private static final String PLAYER_SCRIPT = """
			<script>(function () {
			var V = window.SAIL_VIDEO, F = V.frames, S = V.steps, i = 0, playing = false, timer = null, pin = -1, shown = -2;
			function $(id) { return document.getElementById(id); }
			function el(tag, cls, text) { var e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; }
			function secs(ms) { return (ms / 1000).toFixed(1) + ' s'; }
			function dur(ms) { return ms < 1000 ? ms + ' ms' : secs(ms); }
			var img = $('frame'), cap = $('caption'), seek = $('seek'), play = $('play'), speed = $('speed'), clock = $('clock'), list = $('steps');
			var t0 = F.length ? F[0][1] : 0, end = F.length ? F[F.length - 1][1] - t0 : 0, items = [], group = null, ol = null;
			seek.max = Math.max(F.length - 1, 0);
			S.forEach(function (s, k) {
				if (s.p !== group) { group = s.p; var section = el('section'); section.append(el('h3', null, s.p)); ol = el('ol'); section.append(ol); list.append(section); }
				var li = el('li', s.f ? 'fail' : null); li.value = s.n; li.title = s.x;
				li.append(el('div', 'summary', s.d || s.x));
				if (s.f) { li.append(el('div', 'failure', s.f)); }
				li.onclick = function () { jump(k); };
				ol.append(li); items.push(li);
			});
			/* the last frame drawn at or before t */
			function frameAt(t) { var lo = 0, hi = F.length - 1; if (hi < 0) return 0; while (lo < hi) { var m = (lo + hi + 1) >> 1; if (F[m][1] <= t) lo = m; else hi = m - 1; } return lo; }
			/* the first frame drawn at or after t (F.length if none) */
			function frameFrom(t) { var lo = 0, hi = F.length; while (lo < hi) { var m = (lo + hi) >> 1; if (F[m][1] < t) lo = m + 1; else hi = m; } return lo; }
			function stepAt(t) { var r = -1; for (var k = 0; k < S.length; k++) { if (S[k].s <= t) r = k; else break; } return r; }
			function caption(k) {
				var cur = list.querySelector('li.current'); if (cur) cur.classList.remove('current');
				cap.className = 'card caption';
				if (k < 0) { cap.replaceChildren(el('div', 'cap-meta', 'Before the first step')); return; }
				var s = S[k], li = items[k]; li.classList.add('current'); li.scrollIntoView({block: 'nearest'});
				if (s.f) cap.classList.add('fail');
				var meta = el('div', 'cap-meta');
				meta.append(el('span', s.f ? 'badge fail' : 'badge pass', s.f ? 'Failed' : 'Passed'), el('span', null, 'Step ' + s.n + ' of ' + S.length + ' \\u00b7 ' + dur(s.e - s.s)));
				var parts = [meta, el('div', 'cap-group', s.p), el('div', 'cap-summary', s.d || s.x)];
				if (s.f) { parts.push(el('div', 'failure', s.f)); }
				parts.push(el('code', 'xml', s.x));
				cap.replaceChildren.apply(cap, parts);
			}
			function show() {
				if (!F.length) { cap.replaceChildren(el('div', 'cap-meta', 'No frames were recorded.')); return; }
				var t = F[i][1]; img.src = F[i][0]; seek.value = i; clock.textContent = secs(t - t0) + ' / ' + secs(end);
				if (i + 1 < F.length) { new Image().src = F[i + 1][0]; }
				var k = pin >= 0 ? pin : stepAt(t);
				if (k !== shown) { shown = k; caption(k); }
			}
			function go(k) { i = Math.max(0, Math.min(F.length - 1, k)); show(); if (playing) schedule(); }
			/* shows a step: its first frame, or the screen as it was during it when nothing was drawn then */
			function jump(k) {
				var s = S[k], f = frameFrom(s.s);
				if (f >= F.length || F[f][1] > s.e) f = frameAt(s.e);
				pin = k; go(f);
			}
			function schedule() {
				clearTimeout(timer);
				if (i + 1 >= F.length) { pause(); return; }
				timer = setTimeout(function () { i++; pin = -1; show(); schedule(); }, Math.max(0, (F[i+1][1]-F[i][1]) / parseFloat(speed.value)));
			}
			function pause() { playing = false; clearTimeout(timer); play.textContent = 'Play'; }
			play.onclick = function () { if (playing) { pause(); } else { if (i + 1 >= F.length) { i = 0; pin = -1; } playing = true; play.textContent = 'Pause'; schedule(); } };
			$('prev').onclick = function () { if (S.length) jump(Math.max(0, shown - 1)); };
			$('next').onclick = function () { if (S.length) jump(Math.min(S.length - 1, shown + 1)); };
			speed.onchange = function () { if (playing) schedule(); };
			seek.oninput = function () { pin = -1; go(parseInt(seek.value, 10)); };
			document.addEventListener('keydown', function (e) {
				var tag = e.target.tagName;
				if (tag === 'INPUT' || tag === 'SELECT' || tag === 'TEXTAREA' || e.metaKey || e.ctrlKey || e.altKey) return;
				if (e.key === ' ' && tag !== 'BUTTON') { e.preventDefault(); play.click(); }
				else if (e.key === 'ArrowLeft') { e.preventDefault(); $('prev').click(); }
				else if (e.key === 'ArrowRight') { e.preventDefault(); $('next').click(); }
			});
			show();
			})();</script>
			""";

	/**
	 * The list of runs' script: shows only failed runs, or the runs whose text (or file or unit heading) has what is
	 * typed; a heading, or a session, with none of its runs shown is hidden too.
	 */
	private static final String RUNS_SCRIPT = """
			<script>(function () {
			var failed = document.getElementById('failedOnly'), find = document.getElementById('find');
			function all(selector, root) { return Array.prototype.slice.call((root || document).querySelectorAll(selector)); }
			var rows = all('tr[data-run]'), headers = all('tr[data-header]'), sessions = all('section.session'), count = document.getElementById('shown');
			var headerText = {};
			headers.forEach(function (h) { headerText[h.dataset.header] = h.textContent.toLowerCase(); });
			function apply() {
				var text = find.value.trim().toLowerCase(), n = 0, shown = {};
				rows.forEach(function (r) {
					var haystack = r.textContent.toLowerCase() + ' ' + (headerText[r.dataset.g] || '') + ' ' + (headerText[r.dataset.u] || '');
					r.hidden = (failed.checked && r.dataset.failed !== 'true') || (text !== '' && haystack.indexOf(text) < 0);
					if (!r.hidden) { n++; if (r.dataset.g) shown[r.dataset.g] = true; if (r.dataset.u) shown[r.dataset.u] = true; }
				});
				headers.forEach(function (h) { h.hidden = !shown[h.dataset.header]; });
				sessions.forEach(function (s) { s.hidden = all('tr[data-run]', s).every(function (r) { return r.hidden; }); });
				count.textContent = (n === rows.length) ? '' : n + ' shown';
			}
			failed.onchange = apply; find.oninput = apply;
			})();</script>
			""";

	/** A setting as {@code name=value (source)}, as {@link SailSettings#describe()} writes it. */
	private static final Pattern SETTING = Pattern.compile("^([\\w.]+)=(.*) \\(([^()]*)\\)$", Pattern.DOTALL);

	/** Starts a page: its head, with the shared style (and any of its own), and its body. */
	private static void startPage(@Nonnull StringBuilder out, @Nonnull String title, @Nonnull String extraHead, @Nullable String bodyClass) {
		out.append("<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">")
			.append("<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">")
			.append("<title>").append(escape(title)).append("</title>").append(STYLE).append(extraHead).append("</head><body")
			.append((bodyClass == null) ? "" : " class=\"" + bodyClass + '"').append('>');
	}

	/** A run's outcome as a badge: passed, or how many steps failed. */
	private static @Nonnull String outcomeBadge(int failed) {
		return (failed == 0) ? "<span class=\"badge pass\">Passed</span>" : "<span class=\"badge fail\">" + failed + " failed</span>";
	}

	/**
	 * Writes a run page's header: its title, outcome, test and timing; links to its other pages; and a Settings button
	 * that opens the run's details and settings in a pop-up.
	 *
	 * @param out	the page
	 * @param info	the run
	 * @param steps	its steps
	 * @param extraSub	more for the line under the title (HTML), or empty
	 * @param links	the links, as HTML
	 */
	private static void runHeader(@Nonnull StringBuilder out, @Nonnull RunInfo info, @Nonnull List<StepRecord> steps,
									@Nonnull String extraSub, @Nonnull String links) {
		int failed = (int) steps.stream().filter(StepRecord::failed).count();
		out.append("<header class=\"top\"><div class=\"heading\"><h1>").append(escape(info.title())).append("</h1><div class=\"sub\">")
			.append(outcomeBadge(failed))
			.append("<span>").append(escape(info.test())).append((info.label() == null) ? "" : " &middot; " + escape(info.label())).append("</span>")
			.append("<span>").append(escape(info.started())).append("</span>")
			.append("<span>").append(steps.size()).append(" steps &middot; ").append(duration(runMillis(steps))).append("</span>")
			.append(extraSub).append("</div></div><nav class=\"nav\">").append(links)
			.append("<a class=\"btn\" href=\"../index.html\">All runs</a>")
			.append("<button class=\"btn\" type=\"button\" popovertarget=\"settings\">Settings</button></nav></header>");
		settingsPopup(out, info);
	}

	/** Writes the pop-up with a run's details and its settings that are not defaults, with their sources. */
	private static void settingsPopup(@Nonnull StringBuilder out, @Nonnull RunInfo info) {
		out.append("<div id=\"settings\" class=\"pop\" popover><div class=\"pop-head\"><h2>Run details</h2>")
			.append("<button class=\"btn\" type=\"button\" popovertarget=\"settings\" popovertargetaction=\"hide\">Close</button></div><dl>")
			.append("<dt>Test</dt><dd>").append(escape(info.test())).append("</dd>");
		if (info.label() != null) {
			out.append("<dt>Script</dt><dd>").append(escape(info.label())).append("</dd>");
		}
		out.append("<dt>Started</dt><dd>").append(escape(info.started())).append("</dd>")
			.append("<dt>UX/UI</dt><dd>").append(escape(info.uxui())).append("</dd>")
			.append("<dt>Folder</dt><dd>").append(escape(info.name())).append("</dd></dl>")
			.append("<h3>Settings that are not defaults</h3>");
		if (info.settings().isEmpty()) {
			out.append("<p class=\"muted\">Settings: all defaults</p>");
		}
		else {
			out.append("<dl class=\"settings\">");
			for (String setting : info.settings()) {
				Matcher matcher = SETTING.matcher(setting);
				if (matcher.matches()) {
					out.append("<dt>").append(escape(matcher.group(1))).append("</dt><dd>").append(escape(matcher.group(2)))
						.append(" <span class=\"muted\">(").append(escape(matcher.group(3))).append(")</span></dd>");
				}
				else {
					out.append("<dt></dt><dd>").append(escape(setting)).append("</dd>");
				}
			}
			out.append("</dl>");
		}
		out.append("</div>");
	}

	/**
	 * Writes the film strip of a run: one row per executed step, under its interaction.
	 *
	 * @param info	the run
	 * @param steps	the executed steps, in order
	 * @param playerLink	the run's video player, relative to the film strip, or null
	 * @return the page
	 */
	static @Nonnull String filmStripHtml(@Nonnull RunInfo info, @Nonnull List<StepRecord> steps, @Nullable String playerLink) {
		StepRecord firstFailure = steps.stream().filter(StepRecord::failed).findFirst().orElse(null);
		StringBuilder result = new StringBuilder(8192 + (steps.size() * 768));
		startPage(result, info.title() + " · steps", "", null);
		runHeader(result, info, steps,
					(firstFailure == null) ? "" : "<a href=\"#step-" + firstFailure.sequence() + "\">Jump to the first failure</a>",
					(playerLink == null) ? "" : "<a class=\"btn\" href=\"" + escape(playerLink) + "\">Video</a>");
		result.append("<main class=\"content\"><div class=\"card table-wrap\"><table class=\"strip\"><thead><tr><th>#</th><th>Step</th>")
				.append("<th>Outcome</th><th>Time</th><th>Screenshot</th></tr></thead><tbody>");
		String group = null;
		for (StepRecord step : steps) {
			if (! step.procedure().equals(group)) {
				group = step.procedure();
				result.append("<tr class=\"group\"><td colspan=\"5\">").append(escape(group)).append("</td></tr>");
			}
			result.append("<tr id=\"step-").append(step.sequence()).append('"').append(step.failed() ? " class=\"fail\"" : "").append('>')
					.append("<td class=\"num\">").append(step.sequence()).append("</td>")
					.append("<td><div class=\"summary\">").append(escape(summary(step.xml()))).append("</div>");
			if (step.failed()) {
				result.append("<div class=\"failure\">").append(escape(step.failure())).append("</div>");
			}
			result.append("<code class=\"xml\">").append(escape(step.xml())).append("</code></td><td>")
					.append(step.failed() ? "<span class=\"badge fail\">failed</span>" : "<span class=\"badge pass\">passed</span>")
					.append("</td><td class=\"num\">").append(duration(step.endMillis() - step.startMillis())).append("</td><td class=\"shot\">");
			if (step.screenshot() != null) {
				String file = escape(step.screenshot());
				result.append("<a target=\"_blank\" rel=\"noopener\" href=\"").append(file).append("\"><img loading=\"lazy\" src=\"").append(file).append("\" alt=\"step ")
						.append(step.sequence()).append("\"></a>");
			}
			if (step.source() != null) {
				result.append("<a class=\"small\" target=\"_blank\" rel=\"noopener\" href=\"").append(escape(step.source())).append("\">page source</a>");
			}
			result.append("</td></tr>");
		}
		result.append("</tbody></table></div></main></body></html>\n");
		return result.toString();
	}

	/**
	 * Writes the player's data: the frames with their times and the steps with theirs, as a script the static player
	 * loads (a page opened from the file system cannot fetch JSON).
	 *
	 * @param info	the run
	 * @param frames	the frames, in order
	 * @param steps	the executed steps, in order
	 * @param filmStripLink	the run's film strip, relative to the player, or null
	 * @return the script
	 */
	static @Nonnull String videoScript(@Nonnull RunInfo info, @Nonnull List<Frame> frames, @Nonnull List<StepRecord> steps, @Nullable String filmStripLink) {
		List<Map<String, Object>> stepData = new ArrayList<>(steps.size());
		for (StepRecord step : steps) {
			Map<String, Object> data = new LinkedHashMap<>();
			data.put("n", Integer.valueOf(step.sequence()));
			data.put("p", step.procedure());
			data.put("d", summary(step.xml()));
			data.put("x", step.xml());
			data.put("s", Long.valueOf(step.startMillis()));
			data.put("e", Long.valueOf(step.endMillis()));
			if (step.failed()) {
				data.put("f", step.failure());
			}
			stepData.add(data);
		}
		List<Object> frameData = new ArrayList<>(frames.size());
		for (Frame frame : frames) {
			frameData.add(List.of(frame.file(), Long.valueOf(frame.millis())));
		}
		Map<String, Object> video = new LinkedHashMap<>();
		video.put("title", info.title());
		video.put("test", info.test() + ((info.label() == null) ? "" : " · " + info.label()));
		video.put("started", info.started());
		video.put("settings", info.settings());
		video.put("filmStrip", filmStripLink);
		video.put("frames", frameData);
		video.put("steps", stepData);
		return "window.SAIL_VIDEO = " + new Json().toJson(video) + ";\n";
	}

	/**
	 * Writes the player page, which plays {@code video.js}'s frames at their real timing, fitted to the window, with
	 * the running step's caption under them and the steps, by interaction, beside them.
	 *
	 * @param info	the run
	 * @param steps	the executed steps, in order
	 * @param filmStripLink	the run's film strip, relative to the player, or null
	 * @return the page
	 */
	static @Nonnull String playerHtml(@Nonnull RunInfo info, @Nonnull List<StepRecord> steps, @Nullable String filmStripLink) {
		StringBuilder result = new StringBuilder(16384);
		startPage(result, info.title() + " · video", PLAYER_STYLE, "player-page");
		runHeader(result, info, steps, "",
					(filmStripLink == null) ? "" : "<a class=\"btn\" href=\"" + escape(filmStripLink) + "\">Steps</a>");
		result.append("<div class=\"toolbar\"><button id=\"play\" class=\"btn primary\" type=\"button\">Play</button>")
				.append("<button id=\"prev\" class=\"btn\" type=\"button\" title=\"Previous step (left arrow)\">&lsaquo; Step</button>")
				.append("<button id=\"next\" class=\"btn\" type=\"button\" title=\"Next step (right arrow)\">Step &rsaquo;</button>")
				.append("<label>Speed <select id=\"speed\"><option value=\"0.5\">0.5x</option><option value=\"1\" selected>1x</option>")
				.append("<option value=\"2\">2x</option><option value=\"4\">4x</option><option value=\"8\">8x</option></select></label>")
				.append("<input id=\"seek\" type=\"range\" min=\"0\" value=\"0\" aria-label=\"Position\"><span id=\"clock\" class=\"num\"></span></div>")
				.append("<main class=\"stage\"><section class=\"screen\"><div class=\"frame-box\"><img id=\"frame\" alt=\"The browser\"></div>")
				.append("<div id=\"caption\" class=\"card caption\"></div></section>")
				.append("<aside class=\"card steps-panel\"><div class=\"panel-head\">Steps <span class=\"muted\">click to jump; space plays</span></div>")
				.append("<div id=\"steps\"></div></aside></main>")
				.append("<script src=\"video.js\"></script>").append(PLAYER_SCRIPT).append("</body></html>\n");
		return result.toString();
	}

	/**
	 * A run in the list of runs: its folder, and its {@value #RUN_JSON} (null for a run written before that file existed,
	 * or one that cannot be read).
	 */
	record ListedRun(@Nonnull Path folder, @Nullable Map<String, Object> summary) {
		@Nonnull String name() {
			return folder.getFileName().toString();
		}

		@Nonnull String session() {
			Object session = (summary == null) ? null : summary.get("session");
			return (session == null) ? "" : session.toString();
		}

		@Nullable String label() {
			Object label = (summary == null) ? null : summary.get("label");
			return (label == null) ? null : label.toString();
		}

		long unit() {
			return (summary == null) ? 0L : number(summary.get("unit"));
		}

		@Nonnull String title() {
			Object title = (summary == null) ? null : summary.get("title");
			return (title == null) ? name() : title.toString();
		}

		boolean failed() {
			return (summary != null) && Boolean.FALSE.equals(summary.get("passed"));
		}

		long millis() {
			return (summary == null) ? 0L : number(summary.get("millis"));
		}
	}

	/**
	 * Writes a directory's list of runs: by session (a JVM's run of tests), newest first, and in each session the runs
	 * in the order they ran, as the suite ran them. A suite file's runs come under its name, and the legs of a unit (a
	 * workflow) under the unit. Each run shows its title, test, outcome, steps and time (from its {@value #RUN_JSON};
	 * runs written before that file existed come last, by their folders' names), with its film strip and player. It can
	 * show only the failed runs, or the runs with some text.
	 *
	 * @param base	the directory
	 * @return the page
	 * @throws IOException	if the directory cannot be read
	 */
	static @Nonnull String runsIndexHtml(@Nonnull Path base) throws IOException {
		List<ListedRun> runs = new ArrayList<>();
		try (DirectoryStream<Path> children = Files.newDirectoryStream(base, Files::isDirectory)) {
			for (Path child : children) {
				if (Files.exists(child.resolve("index.html")) || Files.exists(child.resolve("player.html"))) {
					runs.add(new ListedRun(child, readRunJson(child)));
				}
			}
		}
		// sessions newest first (runs without one last), and each session's runs in the order they started
		runs.sort(Comparator.comparing((ListedRun r) -> r.session().isEmpty() ? "" : r.session(), Comparator.reverseOrder())
							.thenComparing(r -> r.session().isEmpty() ? "" : r.name())
							.thenComparing(ListedRun::name, Comparator.reverseOrder()));
		long failedRuns = runs.stream().filter(ListedRun::failed).count();
		StringBuilder result = new StringBuilder(4096 + (runs.size() * 768));
		startPage(result, "SAIL runs", "", null);
		result.append("<header class=\"top\"><div class=\"heading\"><h1>SAIL runs</h1><div class=\"sub\"><span>")
				.append(runs.size()).append(" runs, newest session first</span>")
				.append((failedRuns == 0) ? "" : "<span class=\"badge fail\">" + failedRuns + " failed</span>")
				.append("<span id=\"shown\"></span></div></div><nav class=\"nav\">")
				.append("<label class=\"btn\"><input type=\"checkbox\" id=\"failedOnly\"> Failed only</label>")
				.append("<input id=\"find\" class=\"btn\" type=\"search\" placeholder=\"Filter runs\" aria-label=\"Filter runs\"></nav></header>")
				.append("<main class=\"content\">");
		int group = 0;
		for (int i = 0; i < runs.size(); ) {
			String session = runs.get(i).session();
			int end = i;
			while ((end < runs.size()) && runs.get(end).session().equals(session)) {
				end++;
			}
			List<ListedRun> sessionRuns = runs.subList(i, end);
			group = sessionHtml(result, session, sessionRuns, group);
			i = end;
		}
		result.append("</main>").append(RUNS_SCRIPT).append("</body></html>\n");
		return result.toString();
	}

	/**
	 * Writes one session's table: its runs in order, under their suite files and units.
	 *
	 * @return the next group number (groups number the file and unit headings across the page, for the filters)
	 */
	private static int sessionHtml(@Nonnull StringBuilder out, @Nonnull String session, @Nonnull List<ListedRun> runs, int firstGroup) {
		int group = firstGroup;
		long failed = runs.stream().filter(ListedRun::failed).count();
		Set<String> tests = new java.util.LinkedHashSet<>();
		for (ListedRun run : runs) {
			Map<String, Object> summary = run.summary();
			Object test = (summary == null) ? null : summary.get("test");
			if (test != null) {
				tests.add(test.toString());
			}
		}
		out.append("<section class=\"session\"><h2>")
			.append(session.isEmpty() ? "Earlier runs" : "Session started " + escape(startedFromFolder(session)))
			.append(" <span class=\"muted\">").append(runs.size()).append(" runs")
			.append(tests.isEmpty() ? "" : " &middot; " + escape(String.join(", ", tests))).append("</span>")
			.append((failed == 0) ? "" : " <span class=\"badge fail\">" + failed + " failed</span>")
			.append("</h2><div class=\"card table-wrap\"><table class=\"runs\"><thead><tr><th>#</th><th>Started</th><th>Run</th>")
			.append("<th>Result</th><th>Steps</th><th>Time</th><th></th></tr></thead><tbody>");
		String label = null;
		int fileGroup = 0;
		for (int i = 0, sequence = 1; i < runs.size(); ) {
			ListedRun run = runs.get(i);
			String runLabel = run.label();
			if ((runLabel != null) && (! runLabel.equals(label))) {
				label = runLabel;
				fileGroup = ++group;
				out.append("<tr class=\"group\" data-header=\"g").append(fileGroup).append("\"><td colspan=\"7\">")
					.append(escape(label)).append("</td></tr>");
			}
			else if ((runLabel == null) && (label != null)) {
				label = null;
				fileGroup = 0;
			}
			// a unit's legs: the runs after this one in the same unit
			int end = i + 1;
			if (run.unit() != 0L) {
				while ((end < runs.size()) && (runs.get(end).unit() == run.unit()) && runs.get(end).session().equals(run.session())) {
					end++;
				}
			}
			int unitGroup = 0;
			if (end - i > 1) {
				List<ListedRun> legs = runs.subList(i, end);
				unitGroup = ++group;
				boolean unitFailed = legs.stream().anyMatch(ListedRun::failed);
				out.append("<tr class=\"unit").append(unitFailed ? " fail" : "").append("\" data-header=\"g").append(unitGroup)
					.append("\"><td></td><td></td><td><div class=\"summary\">").append(escape(unitTitle(legs)))
					.append("</div><div class=\"muted small\">workflow of ").append(legs.size()).append(" legs</div></td><td>")
					.append(unitFailed ? "<span class=\"badge fail\">Failed</span>" : "<span class=\"badge pass\">Passed</span>")
					.append("</td><td></td><td class=\"num\">")
					.append(duration(legs.stream().mapToLong(ListedRun::millis).sum())).append("</td><td></td></tr>");
			}
			for (int j = i; j < end; j++) {
				runRowHtml(out, runs.get(j), sequence++, fileGroup, unitGroup, (unitGroup == 0) ? 0 : j - i + 1);
			}
			i = end;
		}
		out.append("</tbody></table></div></section>");
		return group;
	}

	/** A unit's title: the words its legs' titles start with (e.g. the test's id), or its first leg's title. */
	static @Nonnull String unitTitle(@Nonnull List<ListedRun> legs) {
		String[] common = legs.get(0).title().split(" ");
		int length = common.length;
		for (ListedRun leg : legs) {
			String[] words = leg.title().split(" ");
			int k = 0;
			while ((k < length) && (k < words.length) && common[k].equals(words[k])) {
				k++;
			}
			length = k;
		}
		String result = String.join(" ", java.util.Arrays.copyOf(common, length));
		int end = result.length();
		while ((end > 0) && (Character.isWhitespace(result.charAt(end - 1)) || (":;,.-".indexOf(result.charAt(end - 1)) >= 0))) {
			end--; // no trailing punctuation, as in "HM2:"
		}
		return (end == 0) ? legs.get(0).title() : result.substring(0, end);
	}

	/** Writes one run's row; {@code leg} is its number in its unit, or 0. */
	private static void runRowHtml(@Nonnull StringBuilder out, @Nonnull ListedRun run, int sequence, int fileGroup, int unitGroup, int leg) {
		Map<String, Object> summary = run.summary();
		boolean failed = run.failed();
		out.append("<tr data-run data-failed=\"").append(failed).append('"')
			.append((fileGroup == 0) ? "" : " data-g=\"g" + fileGroup + '"')
			.append((unitGroup == 0) ? "" : " data-u=\"g" + unitGroup + '"');
		String classes = (failed ? "fail" : "") + ((leg == 0) ? "" : (failed ? " leg" : "leg"));
		out.append(classes.isEmpty() ? "" : " class=\"" + classes + '"').append("><td class=\"num muted\">").append(sequence).append("</td>");
		if (summary == null) {
			out.append("<td class=\"num\">").append(escape(startedFromFolder(run.name()))).append("</td><td><div class=\"summary\">")
				.append(escape(run.name())).append("</div></td><td><span class=\"badge\">unknown</span></td><td></td><td></td>");
		}
		else {
			String started = String.valueOf(summary.get("started"));
			Object label = summary.get("label");
			long failedSteps = number(summary.get("failed"));
			out.append("<td class=\"num\">").append(escape(run.session().isEmpty() ? started : started.substring(started.indexOf(' ') + 1)))
				.append("</td><td>").append((leg == 0) ? "" : "<span class=\"leg-no\">leg " + leg + "</span>")
				.append("<div class=\"summary\">").append(escape(run.title())).append("</div><div class=\"muted small\">")
				.append(escape(String.valueOf(summary.get("test")))).append((label == null) ? "" : " &middot; " + escape(String.valueOf(label)))
				.append("</div></td><td>")
				.append(failed ? "<span class=\"badge fail\">" + ((failedSteps == 0) ? "Failed" : failedSteps + " failed") + "</span>"
								: "<span class=\"badge pass\">Passed</span>")
				.append("</td><td class=\"num\">").append(number(summary.get("steps")))
				.append("</td><td class=\"num\">").append(duration(run.millis())).append("</td>");
		}
		String href = escape(run.name());
		out.append("<td class=\"links\">");
		if (Files.exists(run.folder().resolve("index.html"))) {
			out.append("<a class=\"btn\" href=\"").append(href).append("/index.html\">Steps</a>");
		}
		if (Files.exists(run.folder().resolve("player.html"))) {
			out.append("<a class=\"btn\" href=\"").append(href).append("/player.html\">Video</a>");
		}
		out.append("</td></tr>");
	}

	/** Reads a run folder's {@value #RUN_JSON}, or null when it has none or it cannot be read. */
	private static @Nullable Map<String, Object> readRunJson(@Nonnull Path run) {
		Path file = run.resolve(RUN_JSON);
		if (! Files.exists(file)) {
			return null;
		}
		try {
			return new Json().toType(Files.readString(file, StandardCharsets.UTF_8), Json.MAP_TYPE);
		}
		catch (@SuppressWarnings("unused") IOException | RuntimeException e) { // a run being written, or a damaged file: shown by its folder's name
			return null;
		}
	}

	private static long number(@Nullable Object value) {
		return (value instanceof Number n) ? n.longValue() : 0L;
	}

	/** When a run started, from its folder's name ({@code yyyyMMdd-HHmmss-SSS-...}); empty if it is not named so. */
	private static @Nonnull String startedFromFolder(@Nonnull String folder) {
		Matcher matcher = Pattern.compile("^(\\d{4})(\\d{2})(\\d{2})-(\\d{2})(\\d{2})(\\d{2})").matcher(folder);
		return matcher.find() ? matcher.group(1) + '-' + matcher.group(2) + '-' + matcher.group(3) + ' ' +
								matcher.group(4) + ':' + matcher.group(5) + ':' + matcher.group(6) : "";
	}

	/**
	 * Chrome's DevTools screencast of one browser window: each frame is written to {@code frames/} with the time Chrome
	 * drew it, and acknowledged so that Chrome sends the next.
	 * <p>
	 * It opens its own CDP connection ({@link SeleniumCdpConnection}) and speaks raw CDP: it attaches to the window's
	 * target with {@code Target.attachToTarget}, then sends {@code Page.*} commands and listens for
	 * {@code Page.screencastFrame} by name. Selenium's typed DevTools would need a {@code selenium-devtools} module for
	 * the browser's version, and Chrome is newer than the newest on the classpath, where Selenium falls back to a no-op.
	 */
	static final class Screencast {
		@SuppressWarnings("resource") // closed by stop()
		private final Connection connection;
		private final SessionID session;
		private final Path folder;
		private final List<Frame> frames;
		private final Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide;
		/** Writes frames in order, off the test thread and off the CDP connection's thread. */
		private final ExecutorService worker = Executors.newSingleThreadExecutor(r -> {
			Thread result = new Thread(r, "sail-screencast");
			result.setDaemon(true);
			return result;
		});

		private Screencast(@Nonnull Connection connection,
							@Nonnull SessionID session,
							@Nonnull Path folder,
							@Nonnull List<Frame> frames,
							@Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide) {
			this.connection = connection;
			this.session = session;
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
		 * @return the screencast, or null when the browser has no CDP (not Chromium) or it cannot start
		 */
		@SuppressWarnings("resource") // the connection is the screencast's, which stop() closes; closed here if it cannot start
		static @Nullable Screencast start(@Nonnull WebDriver driver,
											@Nonnull Path folder,
											@Nonnull List<Frame> frames,
											@Nonnull Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?> selenide) {
			if (! (driver instanceof HasDevTools)) {
				selenide.trace("SAIL video is not supported for " + driver.getClass().getSimpleName() +
								" (it needs Chromium's DevTools); carrying on without it");
				return null;
			}
			Connection connection = null;
			try {
				connection = SeleniumCdpConnection.create(driver).orElse(null);
				if (connection == null) {
					selenide.trace("SAIL video is not supported: the browser offers no CDP connection; carrying on without it");
					return null;
				}
				// A Chrome window handle is its CDP target id
				Map<String, Object> attached = connection.sendAndWait(null,
																		new Command<Map<String, Object>>("Target.attachToTarget",
																											Map.of("targetId", driver.getWindowHandle(),
																													"flatten", Boolean.TRUE),
																											Json.MAP_TYPE),
																		selenide.timeout());
				SessionID session = new SessionID(String.valueOf(attached.get("sessionId")));
				Screencast result = new Screencast(connection, session, folder, frames, selenide);
				connection.addListener(new Event<>("Page.screencastFrame", Screencast::readFrame), (sequence, frame) -> result.received(frame));
				connection.sendAndWait(session,
										new Command<>("Page.startScreencast", Map.of("format", FRAME_FORMAT,
																					"quality", Integer.valueOf(FRAME_QUALITY),
																					"maxWidth", Integer.valueOf(FRAME_MAX_WIDTH),
																					"maxHeight", Integer.valueOf(FRAME_MAX_HEIGHT))),
										selenide.timeout());
				selenide.trace(String.format("SAIL video: Chrome DevTools screencast over raw CDP, %s quality %d, at most %dx%d",
												FRAME_FORMAT, Integer.valueOf(FRAME_QUALITY), Integer.valueOf(FRAME_MAX_WIDTH), Integer.valueOf(FRAME_MAX_HEIGHT)));
				return result;
			}
			catch (RuntimeException e) {
				selenide.trace("SAIL video could not start, so this run has none: " + e);
				if (connection != null) {
					connection.close();
				}
				return null;
			}
		}

		@SuppressWarnings("unchecked")
		private static @Nonnull Map<String, Object> readFrame(@Nonnull JsonInput input) {
			return (Map<String, Object>) input.read(Json.MAP_TYPE);
		}

		/**
		 * Called on the CDP connection's thread for each frame: acknowledges it (asynchronously, so that Chrome sends the
		 * next one) and hands it to the worker to write.
		 */
		private void received(@Nonnull Map<String, Object> frame) {
			Object sessionId = frame.get("sessionId");
			if (sessionId != null) {
				connection.send(session, new Command<>("Page.screencastFrameAck", Map.of("sessionId", sessionId)));
			}
			try {
				worker.execute(() -> write(frame));
			}
			catch (@SuppressWarnings("unused") RejectedExecutionException e) {
				// the screencast has stopped; a frame already on its way is dropped
			}
		}

		private void write(@Nonnull Map<String, Object> frame) {
			try {
				Object data = frame.get("data");
				Object metadata = frame.get("metadata");
				Object timestamp = (metadata instanceof Map<?, ?> m) ? m.get("timestamp") : null;
				long millis = (timestamp instanceof Number n) ? Math.round(n.doubleValue() * 1000.0) : System.currentTimeMillis();
				if (data instanceof String base64) {
					String file;
					synchronized (frames) {
						file = String.format("frames/%06d.jpg", Integer.valueOf(frames.size() + 1));
						frames.add(new Frame(file, millis));
					}
					Files.write(folder.resolve(file), Base64.getDecoder().decode(base64));
				}
			}
			catch (IOException | RuntimeException e) {
				selenide.trace("Could not save a SAIL video frame: " + e);
			}
		}

		/**
		 * Stops the screencast, waits for the frames already received to be written, and closes the connection.
		 */
		void stop() {
			try {
				connection.sendAndWait(session, new Command<>("Page.stopScreencast", Map.of()), selenide.timeout());
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
			connection.close();
		}
	}
}
