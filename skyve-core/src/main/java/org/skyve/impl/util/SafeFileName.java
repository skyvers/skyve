package org.skyve.impl.util;

import java.text.Normalizer;
import java.util.regex.Pattern;

/**
 * Utility class for sanitising file names to be safe for use in browser-accessible content paths.
 */
public class SafeFileName {

	private static final int MAX_LENGTH = 255;

	// Windows invalid filename characters
	private static final Pattern INVALID_CHARS = Pattern.compile("[\\\\/:*?\"<>|]+");

	// Control characters (0x00–0x1F and 0x7F)
	private static final Pattern CONTROL_CHARS = Pattern.compile("[\\p{Cntrl}]");

	// Boutique/fancy Unicode dashes (e.g., -)
	private static final Pattern BOUTIQUE_DASHES = Pattern.compile("[\u2010\u2011\u2012\u2013\u2014\u2015\u2212]");

	// Windows reserved device names
	private static final Pattern RESERVED = Pattern.compile("^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])$", Pattern.CASE_INSENSITIVE);

	private SafeFileName() {
		// Nothing to see here
	}

	/**
	 * Sanitises the input string into a safe file name by normalising Unicode,
	 * replacing invalid characters, avoiding reserved names, and enforcing length limits.
	 *
	 * @param input the original file name string
	 * @return a sanitised, safe file name string
	 */
	public static String sanitise(String input) {
		if (input == null || input.isBlank()) {
			return "unnamed";
		}

		// 1. Normalize Unicode to NFC (important for macOS)
		String s = Normalizer.normalize(input, Normalizer.Form.NFC);

		// 2. Replace boutique Unicode dashes
		s = BOUTIQUE_DASHES.matcher(s).replaceAll("-");

		// 3. Strip control characters
		s = CONTROL_CHARS.matcher(s).replaceAll("");

		// 4. Remove Windows-invalid chars
		s = INVALID_CHARS.matcher(s).replaceAll("");

		// 5. Trim whitespace
		s = s.trim();

		// 6. Split base and extension
		int dot = s.lastIndexOf('.');
		String base = (dot > 0 ? s.substring(0, dot) : s);
		String ext = (dot > 0 ? s.substring(dot + 1) : "");

		if (base.isBlank()) {
			base = "file";
		}

		// 7. Remove trailing dots/spaces (Windows forbidden)
		base = base.replaceAll("[ .]+$", "");
		if (base.isBlank()) {
			base = "file";
		}

		// 8. Avoid Windows reserved names
		if (RESERVED.matcher(base).matches()) {
			base = base + "_";
		}

		// 9. Additional Unicode-hardening: remove non-printables
		base = base.replaceAll("[^\\p{Print}]", "");

		// 10. Clean extension
		ext = ext.replaceAll("[^A-Za-z0-9]+", "").toLowerCase();

		// 11. Reassemble
		String filename = ext.isBlank() ? base : (base + "." + ext);

		// 12. Enforce maximum length (255) WITHOUT breaking extension
		filename = trimToMax(filename, ext);

		return filename;
	}

	/**
	 * Trims the filename to the maximum allowed length without breaking
	 * the file extension or Unicode surrogate pairs.
	 *
	 * @param name the full filename including extension
	 * @param ext the file extension (without dot)
	 * @return a trimmed filename that respects length and encoding constraints
	 */
	private static String trimToMax(String name, String ext) {
		if (name.length() <= MAX_LENGTH) {
			return name;
		}

		// Reserve space for ".ext" if extension exists
		String suffix = ext.isBlank() ? "" : ("." + ext);

		int maxBaseLength = MAX_LENGTH - suffix.length();
		if (maxBaseLength < 1) {
			// Should never happen unless extension itself is absurdly long
			return name.substring(0, MAX_LENGTH);
		}

		// Trim base part safely
		String base = name.substring(0, name.length() - suffix.length());
		base = safeUnicodeTrim(base, maxBaseLength);

		return base + suffix;
	}

	/**
	 * Safely trims a string to the specified length without splitting
	 * surrogate pairs used in Unicode encoding.
	 *
	 * @param s the string to trim
	 * @param maxChars maximum allowed length
	 * @return a trimmed string respecting Unicode surrogate pairs
	 */
	private static String safeUnicodeTrim(String s, int maxChars) {
		if (s.length() <= maxChars) {
			return s;
		}

		int cut = maxChars;

		// If cut lands in the middle of a surrogate pair, move back one.
		if (cut > 0 && Character.isHighSurrogate(s.charAt(cut - 1))) {
			cut--;
		}

		return s.substring(0, cut);
	}
}
