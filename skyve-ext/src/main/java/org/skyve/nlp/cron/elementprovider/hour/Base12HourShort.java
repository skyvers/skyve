package org.skyve.nlp.cron.elementprovider.hour;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses compact 12-hour values such as {@code 8pm} and normalizes them for cron fields.
 */
public class Base12HourShort extends Base12Hour {

	/**
	 * Regex pattern for compact 12-hour tokens with AM/PM suffixes.
	 */
	private static final String PATTERN = "(1[012]|[1-9])\\s?(?i)\\s?(am|pm)";
	/**
	 * Compiled form of the compact 12-hour regex.
	 */
	private Pattern compiledPattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);

	/**
	 * Captures compact 12-hour tokens with AM/PM suffixes.
	 *
	 * @param value The text to inspect.
	 * @return {@code true} when a compact time token is found.
	 */
	@Override
	public boolean matches(String value) {
		Matcher m = compiledPattern.matcher(value);
		while (m.find()) {
			for (int i = 0; i <= m.groupCount(); i++) {
				if (m.group(i) != null) {
					segments.add(m.group(i));
				}
			}
		}
		return !segments.isEmpty();
	}

	/**
	 * Indicates whether this provider can supply a minute element.
	 *
	 * @return {@code true} when an hour token was captured.
	 */
	@Override
	public boolean canProvideMinute() {
		return segments.size() > 1;
	}

	/**
	 * Returns the minute token for compact 12-hour input.
	 *
	 * @return {@code "0"}.
	 */
	@Override
	public String getMinuteElement() {
		return "0";
	}

	/**
	 * Returns the normalized hour token from compact 12-hour input.
	 *
	 * @return The hour field token, or {@code null} when no hour was captured.
	 */
	@Override
	public String getHourElement() {
		if (segments.size() > 2) {
			if ("pm".equalsIgnoreCase(segments.get(2)) && Integer.parseInt(segments.get(1)) < 12) {
				return String.valueOf(Integer.parseInt(segments.get(1)) + 12);
			} else if ("am".equalsIgnoreCase(segments.get(2)) && Integer.parseInt(segments.get(1)) == 12) {
				return "0";
			}
		}
		return segments.size() > 1 ? segments.get(1) : null;
	}
}
