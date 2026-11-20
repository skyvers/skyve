package org.skyve.metadata.sail.language;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Tidies the text of a SAIL {@code <description>}, which a script writes indented over several lines, into the plain
 * text that reports show: paragraphs, split at blank lines, each on one line with its white space collapsed, joined by
 * a blank line. The text is shown as written: tokens such as {@code {run}} and {@code {var:name}} are not substituted.
 */
public final class Descriptions {
	/** A line break: CRLF, CR or LF. */
	private static final Pattern LINE_BREAK = Pattern.compile("\r\n?");
	/** A blank line: a line break, then any white space that includes another line break (after {@link #LINE_BREAK}). */
	private static final Pattern BLANK_LINE = Pattern.compile("\n[ \t]*\n\\s*");
	/** White space, including the Unicode line and paragraph separators, which a paragraph collapses to one space. */
	private static final Pattern WHITE_SPACE = Pattern.compile("[\\s\u0085\u2028\u2029]+");

	/**
	 * Tidies a description's text.
	 *
	 * @param text	the text as written, or null
	 * @return its paragraphs, each on one line, joined by {@code "\n\n"}; or null when it has no text
	 */
	public static @Nullable String normalise(@Nullable String text) {
		if (text == null) {
			return null;
		}
		List<String> paragraphs = new ArrayList<>();
		String lines = LINE_BREAK.matcher(text.replace('\u00A0', ' ')).replaceAll("\n");
		for (String paragraph : BLANK_LINE.split(lines.strip())) {
			String line = WHITE_SPACE.matcher(paragraph).replaceAll(" ").strip();
			if (! line.isEmpty()) {
				paragraphs.add(line);
			}
		}
		return paragraphs.isEmpty() ? null : String.join("\n\n", paragraphs);
	}

	/**
	 * Splits a tidied description into its paragraphs.
	 *
	 * @param description	a description from {@link #normalise(String)}, or null
	 * @return its paragraphs, in order; empty for null
	 */
	public static @Nonnull List<String> paragraphs(@Nullable String description) {
		return (description == null) ? List.of() : List.of(description.split("\n\n"));
	}

	private Descriptions() {
		// static methods only
	}
}
