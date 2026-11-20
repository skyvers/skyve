package util.sail;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.Locale;

import org.skyve.metadata.sail.language.Descriptions;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * The SAIL media pages' shared parts: the page skeleton with the shared style, escaping and durations as shown, and
 * the pages' styles and scripts. Those live in {@code util/sail/media/} as CSS and JavaScript files, to be edited as
 * such, and are inlined into each page, so the pages stay static and open from the file system.
 */
final class SailMediaHtml {
	/** The pages' shared style: light and dark from the system setting, a header, cards, tables, badges and the pop-up. */
	static final String STYLE = "<style>\n" + resource("pages.css") + "\n</style>\n";
	/** The video player's own style. */
	static final String PLAYER_STYLE = "<style>\n" + resource("player.css") + "\n</style>\n";
	/** The video player's script. */
	static final String PLAYER_SCRIPT = "<script>" + resource("player.js") + "</script>\n";
	/** The list of runs' filters. */
	static final String RUNS_SCRIPT = "<script>" + resource("runs.js") + "</script>\n";

	/** Reads one of the pages' styles or scripts from {@code util/sail/media/} (assembly copies it with the harness). */
	private static @Nonnull String resource(@Nonnull String name) {
		String path = "util/sail/media/" + name;
		try (InputStream stream = SailMediaHtml.class.getClassLoader().getResourceAsStream(path)) {
			if (stream == null) {
				throw new IllegalStateException("The SAIL media resource " + path + " is not on the classpath");
			}
			return new String(stream.readAllBytes(), StandardCharsets.UTF_8).strip();
		}
		catch (IOException e) {
			throw new UncheckedIOException("Could not read the SAIL media resource " + path, e);
		}
	}

	/** Starts a page: its head, with the shared style (and any of its own), and its body. */
	static void startPage(@Nonnull StringBuilder out, @Nonnull String title, @Nonnull String extraHead, @Nullable String bodyClass) {
		out.append("<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">")
			.append("<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">")
			.append("<title>").append(escape(title)).append("</title>").append(STYLE).append(extraHead).append("</head><body")
			.append((bodyClass == null) ? "" : " class=\"" + bodyClass + '"').append('>');
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
	 * Writes a description as HTML: a {@code div} of the given class with a paragraph for each of its paragraphs (see
	 * {@link Descriptions#paragraphs(String)}).
	 *
	 * @param description	a tidied description, or null
	 * @param cssClass	the {@code div}'s class
	 * @return the HTML, or empty when there is no description
	 */
	static @Nonnull String descriptionHtml(@Nullable String description, @Nonnull String cssClass) {
		if (description == null) {
			return "";
		}
		StringBuilder result = new StringBuilder(description.length() + 64);
		result.append("<div class=\"").append(cssClass).append("\">");
		for (String paragraph : Descriptions.paragraphs(description)) {
			result.append("<p>").append(escape(paragraph)).append("</p>");
		}
		return result.append("</div>").toString();
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

	private SailMediaHtml() {
		// static methods only
	}
}
