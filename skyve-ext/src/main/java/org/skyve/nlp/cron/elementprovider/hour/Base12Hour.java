package org.skyve.nlp.cron.elementprovider.hour;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses 12-hour clock values such as {@code 8:30am} and converts them to 24-hour cron fields.
 */
public class Base12Hour extends Base24Hour {

	private static final String PATTERN = "(1[012]|[1-9]):([0-5][0-9])?(?i)\\s?(am|pm)";
	private Pattern compiledPattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);

	/**
	 * Captures 12-hour times with AM/PM suffixes.
	 *
	 * @param value The text to inspect.
	 * @return {@code true} when a matching time token is found.
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
		return segments.size() > 0;
	}

	/**
	 * Converts the matched 12-hour value into a cron-compatible 24-hour token.
	 *
	 * @return The converted hour token, or {@code null} when no hour was captured.
	 */
	@Override
	public String getHourElement() {
		if (segments.size() > 3) {
			if (segments.get(3).toLowerCase().equals("pm") && Integer.parseInt(segments.get(1)) < 12) {
				return String.valueOf(Integer.parseInt(segments.get(1)) + 12);
			} else if (segments.get(3).toLowerCase().equals("am") && Integer.parseInt(segments.get(1)) == 12) {
				return "0";
			}
		}
		return segments.size() > 1 ? segments.get(1) : null;
	}
}
