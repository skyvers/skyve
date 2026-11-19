package util.sail;

import static util.sail.SailMediaHtml.duration;
import static util.sail.SailMediaHtml.escape;
import static util.sail.SailMediaHtml.startPage;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.openqa.selenium.json.Json;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import util.sail.SailMedia.Frame;
import util.sail.SailMedia.RunInfo;
import util.sail.SailMedia.StepRecord;

/**
 * A run's pages, for {@link SailMedia}: the film strip ({@code index.html}) and the video player ({@code player.html})
 * with its data ({@code video.js}). Each has a header with the run's title, outcome and timing, links to the other
 * pages, and a Settings pop-up with the run's details and its settings that are not defaults.
 */
final class SailRunPages {
	/** A setting as {@code name=value (source)}, as {@link SailSettings#describe()} writes it. */
	private static final Pattern SETTING = Pattern.compile("^([\\w.]+)=(.*) \\(([^()]*)\\)$", Pattern.DOTALL);

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
			.append("<span>").append(steps.size()).append(" steps &middot; ").append(duration(SailMedia.runMillis(steps))).append("</span>")
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
					.append("<td><div class=\"summary\">").append(escape(step.summary())).append("</div>");
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
			data.put("d", step.summary());
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
		startPage(result, info.title() + " · video", SailMediaHtml.PLAYER_STYLE, "player-page");
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
				.append("<script src=\"video.js\"></script>").append(SailMediaHtml.PLAYER_SCRIPT).append("</body></html>\n");
		return result.toString();
	}

	private SailRunPages() {
		// static methods only
	}
}
