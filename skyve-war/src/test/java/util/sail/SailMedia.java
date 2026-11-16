package util.sail;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Step snapshots and a video of one automation run, for a person to look through what a SAIL run did.
 * <p>
 * Both are opt-in, by {@link SailSettings} (a directory, or an evidence request); without them {@link #start} returns
 * null and the executor does nothing more.
 * <ul>
 * <li>{@value SailSettings#SNAPSHOT_DIR}: after each step (and when a step fails, before the test strategy acts on
 * it), a screenshot named by sequence, interaction and step, and an {@code index.html} film strip of the steps: the
 * step as SAIL, its outcome and duration, and its screenshot. With {@value SailSettings#SNAPSHOT_SOURCE}{@code =true},
 * also each page's source.</li>
 * <li>{@value SailSettings#VIDEO_DIR}: the browser's frames, from Chrome's DevTools screencast, with their times,
 * and a {@code player.html} that plays them at their real timing, captioned with the SAIL step that was running.</li>
 * </ul>
 * Each automation run gets its own folder under each directory, named by its time, test and (for a suite file) the
 * script, so suites, workflows and ITs with several tests keep every run. Each directory gets an {@code index.html}
 * listing its runs. Everything is static HTML that opens from the file system. The run's pages list its settings that
 * do not come from their defaults, with their sources. With {@value SailSettings#KEEP_MEDIA}{@code =failed}, a run that
 * passed deletes its folders when it ends.
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
	/** The script a suite is running on this thread, to name the run's folder; see {@link #label(String)}. */
	private static final ThreadLocal<String> LABEL = new ThreadLocal<>();

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
	 * @param test	the test, as {@code Class.method}
	 * @param label	the suite file, or null
	 * @param started	when it started, as shown
	 * @param uxui	the automation's UX/UI
	 * @param settings	its settings that do not come from their defaults, each with its source (see {@link SailSettings#describe()})
	 */
	record RunInfo(@Nonnull String name,
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
	private final Path snapshotRun;
	private final Path videoRun;
	private final boolean saveSource;
	private final SailSettings.KeepMedia keepMedia;
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
		String name = sanitise(FOLDER_TIME.format(now) + '-' + test + ((label == null) ? "" : '-' + label), 120);
		RunInfo info = new RunInfo(name, test, label, SHOWN_TIME.format(now), automation.getUxui(), settings.describe());
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
	 * Ends the run: stops the screencast, and writes the film strip, the player and each directory's list of runs; or,
	 * when the run passed and {@value SailSettings#KEEP_MEDIA} is {@code failed}, deletes the run's folders instead.
	 *
	 * <p>Side effects: writes or deletes files in the run's folders, and rewrites each directory's list of runs.
	 *
	 * @param passed	whether the automation passed; a run with a failed step is kept even if this is true
	 */
	void finish(boolean passed) {
		if (screencast != null) {
			screencast.stop();
			screencast = null;
		}
		if (passed && (keepMedia == SailSettings.KeepMedia.FAILED) && steps.stream().noneMatch(StepRecord::failed)) {
			delete();
			return;
		}
		try {
			Path filmStrip = (snapshotRun == null) ? null : snapshotRun.resolve("index.html");
			Path player = (videoRun == null) ? null : videoRun.resolve("player.html");
			if (filmStrip != null) {
				String playerLink = (player == null) ? null : link(snapshotRun, player);
				Files.writeString(filmStrip, filmStripHtml(info, steps, playerLink), StandardCharsets.UTF_8);
				Files.writeString(snapshotRun.getParent().resolve("index.html"), runsIndexHtml(snapshotRun.getParent()), StandardCharsets.UTF_8);
			}
			if (player != null) {
				String filmStripLink = (filmStrip == null) ? null : link(videoRun, filmStrip);
				List<Frame> written;
				synchronized (frames) {
					written = new ArrayList<>(frames);
				}
				Files.writeString(videoRun.resolve("video.js"), videoScript(info, written, steps, filmStripLink), StandardCharsets.UTF_8);
				Files.writeString(player, playerHtml(info), StandardCharsets.UTF_8);
				Files.writeString(videoRun.getParent().resolve("index.html"), runsIndexHtml(videoRun.getParent()), StandardCharsets.UTF_8);
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
				Path index = folder.getParent().resolve("index.html");
				if (Files.exists(index)) {
					Files.writeString(index, runsIndexHtml(folder.getParent()), StandardCharsets.UTF_8);
				}
			}
			catch (IOException | RuntimeException e) {
				selenide.trace("Could not delete the SAIL media of a passed run (" + SailSettings.KEEP_MEDIA + "=failed) in " + folder + ": " + e);
			}
		}
		selenide.trace("SAIL media deleted, as the run passed (" + SailSettings.KEEP_MEDIA + "=failed): " + folders);
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

	private static final String STYLE = "<style>" +
			"body{font:14px/1.4 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;margin:16px;color:#222;background:#fafafa}" +
			"h1{font-size:20px;margin:0 0 4px}.meta{color:#666;margin-bottom:12px}a{color:#0b62c4}" +
			"table{border-collapse:collapse;width:100%}th,td{border-bottom:1px solid #ddd;padding:6px 8px;vertical-align:top;text-align:left}" +
			"th{background:#eee;position:sticky;top:0}code{font:12px/1.4 Menlo,Consolas,monospace;white-space:pre-wrap;word-break:break-all}" +
			"tr.fail td{background:#fde8e8}.pass{color:#16794a;font-weight:600}.failed{color:#b3261e;font-weight:600}" +
			"td.shot img{max-width:360px;border:1px solid #ccc}.num{white-space:nowrap}" +
			"@media (prefers-color-scheme: dark){body{background:#1d1f21;color:#ddd}th{background:#2b2d30}th,td{border-color:#3a3d41}" +
			"tr.fail td{background:#4a2224}.meta{color:#999}a{color:#7ab4ff}}" +
			"</style>";

	/**
	 * Writes the film strip of a run: one row per executed step.
	 *
	 * @param info	the run
	 * @param steps	the executed steps, in order
	 * @param playerLink	the run's video player, relative to the film strip, or null
	 * @return the page
	 */
	static @Nonnull String filmStripHtml(@Nonnull RunInfo info, @Nonnull List<StepRecord> steps, @Nullable String playerLink) {
		int failed = 0;
		StepRecord firstFailure = null;
		for (StepRecord step : steps) {
			if (step.failed()) {
				failed++;
				if (firstFailure == null) {
					firstFailure = step;
				}
			}
		}
		StringBuilder result = new StringBuilder(4096 + (steps.size() * 512));
		result.append("<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">")
				.append("<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">")
				.append("<title>SAIL run ").append(escape(info.name())).append("</title>").append(STYLE).append("</head><body>")
				.append("<h1>").append(escape(info.test())).append((info.label() == null) ? "" : " &middot; " + escape(info.label())).append("</h1>")
				.append("<div class=\"meta\">Started ").append(escape(info.started())).append(" &middot; UX/UI ").append(escape(info.uxui()))
				.append(" &middot; ").append(steps.size()).append(" steps &middot; ");
		if (firstFailure == null) {
			result.append("<span class=\"pass\">no failures</span>");
		}
		else {
			result.append("<span class=\"failed\">").append(failed).append(" failed</span> &middot; <a href=\"#step-")
					.append(firstFailure.sequence()).append("\">jump to the first failure</a>");
		}
		if (playerLink != null) {
			result.append(" &middot; <a href=\"").append(escape(playerLink)).append("\">video</a>");
		}
		result.append(" &middot; <a href=\"../index.html\">all runs</a></div>")
				.append("<div class=\"meta settings\">Settings: ")
				.append(info.settings().isEmpty() ? "all defaults" : String.join(" &middot; ", info.settings().stream().map(SailMedia::escape).toList()))
				.append("</div>")
				.append("<table><thead><tr><th>#</th><th>Interaction</th><th>Step</th><th>Outcome</th><th>Time</th><th>Screenshot</th></tr></thead><tbody>");
		for (StepRecord step : steps) {
			result.append("<tr id=\"step-").append(step.sequence()).append('"').append(step.failed() ? " class=\"fail\"" : "").append('>')
					.append("<td class=\"num\">").append(step.sequence()).append("</td>")
					.append("<td>").append(escape(step.procedure())).append(" <span class=\"meta\">#").append(step.number()).append("</span></td>")
					.append("<td><code>").append(escape(step.xml())).append("</code></td><td>");
			if (step.failed()) {
				result.append("<span class=\"failed\">failed</span><br>").append(escape(step.failure()));
			}
			else {
				result.append("<span class=\"pass\">passed</span>");
			}
			result.append("</td><td class=\"num\">").append(step.endMillis() - step.startMillis()).append(" ms</td><td class=\"shot\">");
			if (step.screenshot() != null) {
				String file = escape(step.screenshot());
				result.append("<a href=\"").append(file).append("\"><img loading=\"lazy\" src=\"").append(file).append("\" alt=\"step ")
						.append(step.sequence()).append("\"></a>");
			}
			if (step.source() != null) {
				result.append("<br><a href=\"").append(escape(step.source())).append("\">page source</a>");
			}
			result.append("</td></tr>");
		}
		result.append("</tbody></table></body></html>\n");
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
			Map<String, Object> data = new java.util.LinkedHashMap<>();
			data.put("n", Integer.valueOf(step.sequence()));
			data.put("p", step.procedure());
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
		Map<String, Object> video = new java.util.LinkedHashMap<>();
		video.put("title", info.test() + ((info.label() == null) ? "" : " · " + info.label()));
		video.put("started", info.started());
		video.put("settings", info.settings());
		video.put("filmStrip", filmStripLink);
		video.put("frames", frameData);
		video.put("steps", stepData);
		return "window.SAIL_VIDEO = " + new Json().toJson(video) + ";\n";
	}

	/**
	 * Writes the player page, which plays {@code video.js}'s frames at their real timing.
	 *
	 * @param info	the run
	 * @return the page
	 */
	static @Nonnull String playerHtml(@Nonnull RunInfo info) {
		return "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">" +
				"<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">" +
				"<title>SAIL video " + escape(info.name()) + "</title>" + STYLE +
				"<style>#wrap{display:flex;gap:12px;align-items:flex-start}#screen{flex:1;min-width:0}" +
				"#frame{max-width:100%;border:1px solid #ccc;background:#fff;display:block}" +
				"#caption{margin:8px 0;min-height:3em}#caption.fail code{color:#b3261e}" +
				"#controls{display:flex;gap:8px;align-items:center;flex-wrap:wrap;margin-bottom:8px}#seek{flex:1;min-width:160px}" +
				"#steps{width:340px;max-height:80vh;overflow:auto;margin:0;padding:0 0 0 28px;font-size:12px}" +
				"#steps li{cursor:pointer;padding:2px 4px}#steps li.current{background:#dbe9fb}#steps li.fail{color:#b3261e}" +
				"@media (max-width:900px){#wrap{flex-direction:column}#steps{width:auto}}" +
				"@media (prefers-color-scheme: dark){#steps li.current{background:#2d4566}}</style>" +
				"</head><body><h1 id=\"title\"></h1><div class=\"meta\" id=\"meta\"></div>" +
				"<div id=\"controls\"><button id=\"play\" type=\"button\">Play</button>" +
				"<label>Speed <select id=\"speed\"><option value=\"0.5\">0.5x</option><option value=\"1\" selected>1x</option>" +
				"<option value=\"2\">2x</option><option value=\"4\">4x</option></select></label>" +
				"<input id=\"seek\" type=\"range\" min=\"0\" value=\"0\"><span id=\"clock\" class=\"num\"></span></div>" +
				"<div id=\"wrap\"><div id=\"screen\"><img id=\"frame\" alt=\"browser frame\"><div id=\"caption\"></div></div>" +
				"<ol id=\"steps\"></ol></div>" +
				"<script src=\"video.js\"></script>" +
				"<script>(function(){" +
				"var V=window.SAIL_VIDEO,F=V.frames,S=V.steps,i=0,playing=false,timer=null;" +
				"var img=document.getElementById('frame'),cap=document.getElementById('caption'),seek=document.getElementById('seek')," +
				"play=document.getElementById('play'),speed=document.getElementById('speed'),clock=document.getElementById('clock')," +
				"list=document.getElementById('steps');" +
				"document.getElementById('title').textContent=V.title;" +
				"var meta=document.getElementById('meta');meta.textContent='Started '+V.started+' \\u00b7 '+S.length+' steps \\u00b7 '+F.length+' frames';" +
				"if(V.filmStrip){var a=document.createElement('a');a.href=V.filmStrip;a.textContent='film strip';meta.append(' \\u00b7 ',a);}" +
				"var all=document.createElement('a');all.href='../index.html';all.textContent='all runs';meta.append(' \\u00b7 ',all);" +
				"var st=document.createElement('div');st.className='meta settings';" +
				"st.textContent='Settings: '+(V.settings.length?V.settings.join(' \\u00b7 '):'all defaults');meta.after(st);" +
				"var t0=F.length?F[0][1]:0;seek.max=Math.max(F.length-1,0);" +
				"S.forEach(function(s,k){var li=document.createElement('li');li.value=s.n;li.textContent=s.p+': '+s.x;" +
				"if(s.f){li.className='fail';li.title=s.f;}li.onclick=function(){go(frameAt(s.s));};list.appendChild(li);});" +
				"function frameAt(t){var lo=0,hi=F.length-1;if(hi<0)return 0;while(lo<hi){var m=(lo+hi+1)>>1;if(F[m][1]<=t)lo=m;else hi=m-1;}return lo;}" +
				"function stepAt(t){var r=-1;for(var k=0;k<S.length;k++){if(S[k].s<=t)r=k;else break;}return r;}" +
				"var shown=-2;function show(){if(!F.length){cap.textContent='No frames were recorded.';return;}" +
				"var t=F[i][1];img.src=F[i][0];seek.value=i;clock.textContent=((t-t0)/1000).toFixed(1)+' s';" +
				"if(i+1<F.length){new Image().src=F[i+1][0];}" +
				"var k=stepAt(t);if(k!==shown){shown=k;var cur=list.querySelector('li.current');if(cur)cur.classList.remove('current');" +
				"if(k<0){cap.textContent='Before the first step';cap.className='';}else{var s=S[k];var li=list.children[k];li.classList.add('current');" +
				"li.scrollIntoView({block:'nearest'});cap.className=s.f?'fail':'';cap.innerHTML='';var b=document.createElement('b');" +
				"b.textContent='Step '+s.n+' \\u00b7 '+s.p+(s.f?' \\u00b7 failed: '+s.f:'');var c=document.createElement('code');" +
				"c.textContent=s.x;cap.append(b,document.createElement('br'),c);}}}" +
				"function go(k){i=Math.max(0,Math.min(F.length-1,k));show();if(playing){schedule();}}" +
				"function schedule(){clearTimeout(timer);if(i+1>=F.length){pause();return;}" +
				"timer=setTimeout(function(){i++;show();schedule();},Math.max(0,(F[i+1][1]-F[i][1])/parseFloat(speed.value)));}" +
				"function pause(){playing=false;clearTimeout(timer);play.textContent='Play';}" +
				"play.onclick=function(){if(playing){pause();}else{if(i+1>=F.length)i=0;playing=true;play.textContent='Pause';schedule();}};" +
				"speed.onchange=function(){if(playing)schedule();};seek.oninput=function(){go(parseInt(seek.value,10));};" +
				"show();})();</script></body></html>\n";
	}

	/**
	 * Writes a directory's list of runs, newest first: each run folder with its film strip and player.
	 *
	 * @param base	the directory
	 * @return the page
	 * @throws IOException	if the directory cannot be read
	 */
	static @Nonnull String runsIndexHtml(@Nonnull Path base) throws IOException {
		List<Path> runs = new ArrayList<>();
		try (DirectoryStream<Path> children = Files.newDirectoryStream(base, Files::isDirectory)) {
			for (Path child : children) {
				if (Files.exists(child.resolve("index.html")) || Files.exists(child.resolve("player.html"))) {
					runs.add(child);
				}
			}
		}
		runs.sort((a, b) -> b.getFileName().toString().compareTo(a.getFileName().toString()));
		StringBuilder result = new StringBuilder(1024 + (runs.size() * 256));
		result.append("<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">")
				.append("<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">")
				.append("<title>SAIL runs</title>").append(STYLE).append("</head><body><h1>SAIL runs</h1><div class=\"meta\">")
				.append(runs.size()).append(" runs, newest first</div><table><thead><tr><th>Run</th><th>Film strip</th><th>Video</th></tr></thead><tbody>");
		for (Path run : runs) {
			String name = escape(run.getFileName().toString());
			result.append("<tr><td>").append(name).append("</td><td>");
			if (Files.exists(run.resolve("index.html"))) {
				result.append("<a href=\"").append(name).append("/index.html\">steps</a>");
			}
			result.append("</td><td>");
			if (Files.exists(run.resolve("player.html"))) {
				result.append("<a href=\"").append(name).append("/player.html\">play</a>");
			}
			result.append("</td></tr>");
		}
		result.append("</tbody></table></body></html>\n");
		return result.toString();
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
