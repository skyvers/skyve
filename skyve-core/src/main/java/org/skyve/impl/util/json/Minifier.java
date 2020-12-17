package org.skyve.impl.util.json;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Minifies JSON file by stripping all whitespace and comments.
 * 
 * @see https://gist.github.com/justisr/abab012af3ef399908798a687185d49a
 */
public class Minifier {

	private Minifier() {
	}

	private static final Map<String, List<String>> COMMENT_INDICATORS = new HashMap<>();
	private static final Map<Character, Character> STRING_CHARS = new HashMap<>();
	private static final Set<Character> ESCAPE_CHARS = new HashSet<>();

	static {
		List<String> COMMENT_CLOSERS = new ArrayList<>();
		COMMENT_CLOSERS.add("\n");
		COMMENT_CLOSERS.add("\r");
		COMMENT_INDICATORS.put("//", COMMENT_CLOSERS);
		COMMENT_CLOSERS = new ArrayList<>();
		COMMENT_CLOSERS.add("*/");
		COMMENT_INDICATORS.put("/*", COMMENT_CLOSERS);
		STRING_CHARS.put('"', '"');
		ESCAPE_CHARS.add('\\');
	}

	/**
	 * Remove single and multi-line comments from any valid java/javascript/json
	 * string
	 * 
	 * @param json string to remove the comments from
	 * @return a minified version of the passed string, with whitespace and comments
	 *         removed, null if param is null.
	 */
	public static final String minify(String json) {
		if (json == null || json.isEmpty())
			return json;
		StringBuilder sb = new StringBuilder();
		String currentCommentIndicator = null;
		Character currentStringChar = null;
		for (int i = 0; i < json.length(); i++) {
			char current = json.charAt(i);
			if (currentStringChar != null) {
				sb.append(current);
				if (current == STRING_CHARS.get(currentStringChar))
					currentStringChar = null;
				if (ESCAPE_CHARS.contains(current) && json.length() > ++i)
					sb.append(json.charAt(i));
			} else if (currentCommentIndicator != null) {
				char lookup;
				lookup: {
					for (String closer : COMMENT_INDICATORS.get(currentCommentIndicator)) {
						for (int x = 0; x < closer.length(); x++) {
							if (json.length() <= i + x)
								break;
							lookup = json.charAt(i + x);
							if (lookup == closer.charAt(x)) {
								if (x + 1 == closer.length()) {
									currentCommentIndicator = null;
									i += x;
									break lookup;
								}
							} else
								break;
						}
					}
				}
			} else if (!Character.isWhitespace(current)) {
				if (STRING_CHARS.containsKey(current))
					currentStringChar = current;
				else {
					char lookup;
					lookup: {
						for (String opener : COMMENT_INDICATORS.keySet()) {
							StringBuilder indicator = null;
							for (int x = 0; x < opener.length(); x++) {
								if (json.length() <= i + x)
									break;
								lookup = json.charAt(i + x);
								if (lookup == opener.charAt(x)) {
									if (indicator == null)
										indicator = new StringBuilder();
									indicator.append(lookup);
									if (x + 1 == opener.length()) {
										currentCommentIndicator = indicator.toString();
										i += x;
										break lookup;
									}
								} else
									break;
							}
						}
					}
				}
				if (currentCommentIndicator == null)
					sb.append(current);
			}
		}
		return sb.toString();
	}
}
