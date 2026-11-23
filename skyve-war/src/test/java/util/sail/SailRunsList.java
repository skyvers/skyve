package util.sail;

import static util.sail.SailMediaHtml.descriptionHtml;
import static util.sail.SailMediaHtml.duration;
import static util.sail.SailMediaHtml.escape;
import static util.sail.SailMediaHtml.startPage;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.openqa.selenium.json.Json;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A directory's list of runs ({@code index.html}), for {@link SailMedia}: by session (a JVM's run of tests), newest
 * first, and in each session the runs in the order they ran, under their suite and its member files, with a workflow's
 * legs under the workflow. The suite, the workflow and each run show their descriptions. It reads each run's
 * {@value SailMedia#RUN_JSON}, and filters by outcome and text. It also writes the runs in that order as a script
 * ({@value SailMedia#RUNS_ORDER_JS}), from which each run page makes its previous and next run buttons.
 */
final class SailRunsList {
	/** The columns of a session's table, by heading; every row has one cell for each. */
	private static final List<String> HEADINGS = List.of("#", "Started", "Run", "Result", "Steps", "Time", "");
	/** A run or session folder's leading time, e.g. {@code 20261106-120000}. */
	private static final Pattern FOLDER_STARTED = Pattern.compile("^(\\d{4})(\\d{2})(\\d{2})-(\\d{2})(\\d{2})(\\d{2})");

	/**
	 * A run in the list of runs: its folder, and its {@value SailMedia#RUN_JSON} (null for a run written before that file existed,
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
			return text("label");
		}

		/** Its suite's file, or null when it is not a suite member. */
		@Nullable String suite() {
			return text("suite");
		}

		/** A text value of its summary, or null when it has none. */
		@Nullable String text(@Nonnull String key) {
			Object value = (summary == null) ? null : summary.get(key);
			return (value == null) ? null : value.toString();
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
	 * Lists a directory's runs in the list's order: sessions newest first (runs without one last), and each session's
	 * runs in the order they started.
	 *
	 * @param base	the directory
	 * @return its runs, each a folder with a film strip or a player
	 * @throws IOException	if the directory cannot be read
	 */
	static @Nonnull List<ListedRun> listedRuns(@Nonnull Path base) throws IOException {
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
		return runs;
	}

	/**
	 * Writes a directory's list of runs, as {@link #listedRuns(Path)} orders them: by session (a JVM's run of tests),
	 * newest first, and in each session the runs in the order they ran, as the suite ran them. A suite file's runs come
	 * under its name, and the legs of a unit (a workflow) under the unit. Each run shows its title, test, outcome, steps
	 * and time (from its {@value SailMedia#RUN_JSON}; runs written before that file existed come last, by their folders'
	 * names), with its film strip and player. It can show only the failed runs, or the runs with some text.
	 *
	 * @param runs	the runs, in the list's order (see {@link #listedRuns(Path)})
	 * @return the page
	 */
	static @Nonnull String runsIndexHtml(@Nonnull List<ListedRun> runs) {
		return runsIndexHtml(runs, "");
	}

	/**
	 * Writes a list of runs, as {@link #runsIndexHtml(List)} does, from a folder other than the runs' directory.
	 *
	 * @param runs	the runs, in the list's order
	 * @param runsLink	the runs' directory relative to the list, ending in {@code /} (such as {@code media/}), or empty
	 *			when the list is in it
	 * @return the page
	 */
	static @Nonnull String runsIndexHtml(@Nonnull List<ListedRun> runs, @Nonnull String runsLink) {
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
			group = sessionHtml(result, session, sessionRuns, group, runsLink);
			i = end;
		}
		result.append("</main>").append(SailMediaHtml.RUNS_SCRIPT).append("</body></html>\n");
		return result.toString();
	}

	/**
	 * Writes one session's table: its runs in order, under their suites, suite files and units. Each suite, file and
	 * unit heading takes the next group number; a run row names its suite's, file's and unit's groups ({@code data-s},
	 * {@code data-g}, {@code data-u}), which is how the filters find the headings to hide with it.
	 *
	 * @param runsLink	the runs' directory relative to the list, or empty
	 * @return the next group number (groups number the file and unit headings across the page, for the filters)
	 */
	private static int sessionHtml(@Nonnull StringBuilder out, @Nonnull String session, @Nonnull List<ListedRun> runs, int firstGroup,
									@Nonnull String runsLink) {
		int group = firstGroup;
		sessionHeadingHtml(out, session, runs);
		String suite = null;
		int suiteGroup = 0;
		String label = null;
		int fileGroup = 0;
		for (int i = 0, sequence = 1; i < runs.size(); ) {
			ListedRun run = runs.get(i);
			String runSuite = run.suite();
			boolean newSuite = ! Objects.equals(runSuite, suite);
			if (newSuite) {
				suite = runSuite;
				suiteGroup = (suite == null) ? 0 : ++group;
				if (suite != null) {
					out.append("<tr class=\"group suite\" data-header=\"g").append(suiteGroup).append("\"><td colspan=\"").append(HEADINGS.size())
						.append("\">Suite ").append(escape(suite)).append(descriptionHtml(run.text("suiteDescription"), "desc")).append("</td></tr>");
				}
			}
			String runLabel = run.label();
			if (newSuite || (! Objects.equals(runLabel, label))) {
				label = runLabel;
				fileGroup = (label == null) ? 0 : ++group;
				if (label != null) {
					out.append("<tr class=\"group\" data-header=\"g").append(fileGroup).append("\"><td colspan=\"").append(HEADINGS.size()).append("\">")
						.append(escape(label)).append("</td></tr>");
				}
			}
			int end = unitEnd(runs, i);
			int unitGroup = 0;
			if (end - i > 1) {
				unitGroup = ++group;
				unitRowHtml(out, runs.subList(i, end), unitGroup);
			}
			for (int j = i; j < end; j++) {
				runRowHtml(out, runs.get(j), new RowPlace(sequence++, suiteGroup, fileGroup, unitGroup, (unitGroup == 0) ? 0 : j - i + 1), runsLink);
			}
			i = end;
		}
		out.append("</tbody></table></div></section>");
		return group;
	}

	/** Writes a session's heading (when it started, how many runs, of which tests, how many failed) and opens its table. */
	private static void sessionHeadingHtml(@Nonnull StringBuilder out, @Nonnull String session, @Nonnull List<ListedRun> runs) {
		long failed = runs.stream().filter(ListedRun::failed).count();
		Set<String> tests = new LinkedHashSet<>();
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
			.append("</h2><div class=\"card table-wrap\"><table class=\"runs\"><thead><tr>");
		HEADINGS.forEach(heading -> out.append("<th>").append(heading).append("</th>"));
		out.append("</tr></thead><tbody>");
	}

	/** Returns the index after the last run of the unit the run at {@code start} is in (just after it, when it is not in one). */
	private static int unitEnd(@Nonnull List<ListedRun> runs, int start) {
		ListedRun run = runs.get(start);
		int result = start + 1;
		if (run.unit() != 0L) {
			while ((result < runs.size()) && (runs.get(result).unit() == run.unit()) && runs.get(result).session().equals(run.session())) {
				result++;
			}
		}
		return result;
	}

	/** Writes a unit's heading row: its title, how many legs, its description, its outcome and total time. */
	private static void unitRowHtml(@Nonnull StringBuilder out, @Nonnull List<ListedRun> legs, int unitGroup) {
		boolean unitFailed = legs.stream().anyMatch(ListedRun::failed);
		out.append("<tr class=\"unit").append(unitFailed ? " fail" : "").append("\" data-header=\"g").append(unitGroup)
			.append("\"><td></td><td></td><td><div class=\"summary\">").append(escape(unitTitle(legs)))
			.append("</div><div class=\"muted small\">workflow of ").append(legs.size()).append(" legs</div>")
			.append(descriptionHtml(legs.get(0).text("workflowDescription"), "desc")).append("</td><td>")
			.append(unitFailed ? "<span class=\"badge fail\">Failed</span>" : "<span class=\"badge pass\">Passed</span>")
			.append("</td><td></td><td class=\"num\">")
			.append(duration(legs.stream().mapToLong(ListedRun::millis).sum())).append("</td><td></td></tr>");
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
		String result = String.join(" ", Arrays.copyOf(common, length));
		int end = result.length();
		while ((end > 0) && (Character.isWhitespace(result.charAt(end - 1)) || (":;,.-".indexOf(result.charAt(end - 1)) >= 0))) {
			end--; // no trailing punctuation, as in "HM2:"
		}
		return (end == 0) ? legs.get(0).title() : result.substring(0, end);
	}

	/**
	 * Where a run's row is in its session's table.
	 *
	 * @param sequence	its number in the session, from 1
	 * @param suiteGroup	its suite's heading's group, or 0 when it has none
	 * @param fileGroup	its suite file's heading's group, or 0 when it has none
	 * @param unitGroup	its unit's heading's group, or 0 when it is not in a unit
	 * @param leg	its number in its unit, from 1; or 0 when it is not in a unit
	 */
	private record RowPlace(int sequence, int suiteGroup, int fileGroup, int unitGroup, int leg) {
		// a value
	}

	/** Writes one run's row, linking to its pages through the runs' directory ({@code runsLink}, or empty). */
	private static void runRowHtml(@Nonnull StringBuilder out, @Nonnull ListedRun run, @Nonnull RowPlace place, @Nonnull String runsLink) {
		Map<String, Object> summary = run.summary();
		boolean failed = run.failed();
		int suiteGroup = place.suiteGroup();
		int fileGroup = place.fileGroup();
		int unitGroup = place.unitGroup();
		int leg = place.leg();
		out.append("<tr data-run data-failed=\"").append(failed).append('"')
			.append((suiteGroup == 0) ? "" : " data-s=\"g" + suiteGroup + '"')
			.append((fileGroup == 0) ? "" : " data-g=\"g" + fileGroup + '"')
			.append((unitGroup == 0) ? "" : " data-u=\"g" + unitGroup + '"');
		String classes = (failed ? "fail" : "") + ((leg == 0) ? "" : (failed ? " leg" : "leg"));
		out.append(classes.isEmpty() ? "" : " class=\"" + classes + '"').append("><td class=\"num muted\">").append(place.sequence()).append("</td>");
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
				.append("</div>")
				// a workflow of one has no unit heading, so its run shows the workflow's description
				.append((leg == 0) ? descriptionHtml(run.text("workflowDescription"), "desc") : "")
				.append(descriptionHtml(run.text("description"), "desc"))
				.append("</td><td>")
				.append(failed ? "<span class=\"badge fail\">" + ((failedSteps == 0) ? "Failed" : failedSteps + " failed") + "</span>"
								: "<span class=\"badge pass\">Passed</span>")
				.append("</td><td class=\"num\">").append(number(summary.get("steps")))
				.append("</td><td class=\"num\">").append(duration(run.millis())).append("</td>");
		}
		String href = escape(runsLink + run.name());
		out.append("<td class=\"links\">");
		if (Files.exists(run.folder().resolve("index.html"))) {
			out.append("<a class=\"btn\" href=\"").append(href).append("/index.html\">Steps</a>");
		}
		if (Files.exists(run.folder().resolve("player.html"))) {
			out.append("<a class=\"btn\" href=\"").append(href).append("/player.html\">Video</a>");
		}
		out.append("</td></tr>");
	}

	/**
	 * Writes {@value SailMedia#RUNS_ORDER_JS}: the runs in the list's order, for each run page's previous and next run
	 * buttons ({@code runnav.js}). A script, not JSON, as a page opened from the file system cannot fetch a file; it
	 * loads it with a {@code script} element.
	 *
	 * @param runs	the runs, in the list's order (see {@link #listedRuns(Path)})
	 * @return the script
	 */
	static @Nonnull String runsOrderScript(@Nonnull List<ListedRun> runs) {
		List<Map<String, Object>> order = new ArrayList<>(runs.size());
		for (ListedRun run : runs) {
			Map<String, Object> entry = new LinkedHashMap<>();
			entry.put("n", run.name());
			entry.put("t", run.title());
			entry.put("e", run.session());
			entry.put("s", Boolean.valueOf(Files.exists(run.folder().resolve("index.html"))));
			entry.put("v", Boolean.valueOf(Files.exists(run.folder().resolve("player.html"))));
			entry.put("f", Boolean.valueOf(run.failed()));
			order.add(entry);
		}
		return "window.SAIL_RUNS = " + new Json().toJson(order) + ";\n";
	}

	/** Reads a run folder's {@value SailMedia#RUN_JSON}, or null when it has none or it cannot be read. */
	private static @Nullable Map<String, Object> readRunJson(@Nonnull Path run) {
		Path file = run.resolve(SailMedia.RUN_JSON);
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
		Matcher matcher = FOLDER_STARTED.matcher(folder);
		return matcher.find() ? matcher.group(1) + '-' + matcher.group(2) + '-' + matcher.group(3) + ' ' +
								matcher.group(4) + ':' + matcher.group(5) + ':' + matcher.group(6) : "";
	}

	private SailRunsList() {
		// static methods only
	}
}
