package org.skyve.domain.types.converters;

import jakarta.annotation.Nonnull;

/**
 * The characters a number converter reads, the same in every JVM locale and in the SmartClient and Vue renderers.
 *
 * <p>Number converters read English numbers ({@code -1,234.50}): {@link java.text.DecimalFormat} and
 * {@link Integer#valueOf(String)} would also read other characters, such as non-ASCII digits ({@code ١٢}), an exponent
 * ({@code 1E3}), or a grouping separator that is not between digits ({@code 1,}, {@code ,1}, {@code 1,.5}), which the
 * browsers refuse.
 *
 * <p>Threading: stateless and thread-safe.
 */
public final class NumberText {
	private NumberText() {
		// static methods only
	}

	/**
	 * Returns whether the text of a number has only the characters a number converter reads: no letters (so no
	 * exponent), only ASCII digits, and each {@code ,} between two digits of the whole part.
	 *
	 * <p>Other characters, such as a sign, a {@code .}, {@code $} or {@code %}, are left to the converter.
	 *
	 * @param text the text, trimmed
	 * @return {@code true} if the text passes these checks
	 */
	public static boolean isPlain(@Nonnull String text) {
		boolean fraction = false;
		for (int i = 0, l = text.length(); i < l; i++) {
			char c = text.charAt(i);
			if (Character.isLetter(c) || (Character.isDigit(c) && (! asciiDigit(c)))) {
				return false;
			}
			if (c == '.') {
				fraction = true;
			}
			else if ((c == ',') &&
						(fraction || (i == 0) || (i == l - 1) || (! asciiDigit(text.charAt(i - 1))) || (! asciiDigit(text.charAt(i + 1))))) {
				return false;
			}
		}
		return true;
	}

	private static boolean asciiDigit(char c) {
		return (c >= '0') && (c <= '9');
	}
}
