package org.skyve.nlp.cron.elementprovider.recurring;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

/**
 * Parses day-based recurrence phrases such as {@code daily} and weekday names.
 */
public class EveryDay implements ExpressionElementProvider {

	/**
	 * Regex pattern that identifies daily and weekday recurrence phrases.
	 */
	private static final String DAY_OF_WEEK_PATTERN = "(daily|(every|each|on)\\s(day|monday|tuesday|wednesday|thursday|friday|saturday|sunday)(?s))";

	/**
	 * Compiled matcher for day-based recurrence phrases.
	 */
	private Pattern dayOfWeekPattern = Pattern.compile(DAY_OF_WEEK_PATTERN, Pattern.CASE_INSENSITIVE);

	/**
	 * Captured regex groups from the last successful match pass.
	 */
	private List<String> segments = new ArrayList<>();
	/**
	 * Maps weekday names to cron day-of-week numeric values.
	 */
	private Map<String, String> dayMap;

	/**
	 * Creates day-name mappings used to translate weekday text into cron day-of-week values.
	 */
	public EveryDay() {
		dayMap = new HashMap<>();

		dayMap.put("sunday", "0");
		dayMap.put("monday", "1");
		dayMap.put("tuesday", "2");
		dayMap.put("wednesday", "3");
		dayMap.put("thursday", "4");
		dayMap.put("friday", "5");
		dayMap.put("saturday", "6");
	}

	/**
	 * Matches daily or weekday recurrence language.
	 *
	 * @param value The text to inspect.
	 * @return {@code true} when a supported day pattern is found.
	 */
	@Override
	public boolean matches(String value) {
		Matcher m = dayOfWeekPattern.matcher(value);
		while (m.find()) {
			for (int i = 0; i <= m.groupCount(); i++) {
				segments.add(m.group(i));
			}
		}
		return !segments.isEmpty();
	}

	/**
	 * Indicates this provider can supply a minute element.
	 *
	 * @return {@code true}.
	 */
	@Override
	public boolean canProvideMinute() {
		return true;
	}

	/**
	 * Returns the default minute element.
	 *
	 * @return {@code "0"}.
	 */
	@Override
	public String getMinuteElement() {
		return "0";
	}

	/**
	 * Indicates this provider can supply an hour element.
	 *
	 * @return {@code true}.
	 */
	@Override
	public boolean canProvideHour() {
		return true;
	}

	/**
	 * Returns the default hour element.
	 *
	 * @return {@code "0"}.
	 */
	@Override
	public String getHourElement() {
		return "0";
	}

	/**
	 * Indicates this provider can supply a day-of-month element.
	 *
	 * @return {@code true}.
	 */
	@Override
	public boolean canProvideDayNumber() {
		return true;
	}

	/**
	 * Returns the day-of-month wildcard element.
	 *
	 * @return {@code "*"}.
	 */
	@Override
	public String getDayNumberElement() {
		return "*";
	}

	/**
	 * Indicates this provider can supply a month element.
	 *
	 * @return {@code true}.
	 */
	@Override
	public boolean canProvideMonth() {
		return true;
	}

	/**
	 * Returns the month wildcard element.
	 *
	 * @return {@code "*"}.
	 */
	@Override
	public String getMonthElement() {
		return "*";
	}

	/**
	 * Indicates whether this provider can supply a day-of-week element.
	 *
	 * @return {@code true} when a day pattern was matched.
	 */
	@Override
	public boolean canProvideDayOfWeek() {
		return !segments.isEmpty();
	}

	/**
	 * Returns the derived day-of-week cron token.
	 *
	 * @return {@code *} for daily, a numeric day-of-week token, or {@code null} when unmatched.
	 */
	@Override
	public String getDayOfWeekElement() {
		if ((!segments.isEmpty() && "daily".equalsIgnoreCase(segments.get(0)))
				|| (segments.size() > 3 && "day".equalsIgnoreCase(segments.get(3)))) {
			return "*";
		}
		return segments.size() > 3 && dayMap.get(segments.get(3).toLowerCase()) != null ? dayMap.get(segments.get(3).toLowerCase())
				: null;
	}

	/**
	 * Indicates whether the minute element is locked.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean isMinuteElementLocked() {
		return false;
	}

	/**
	 * Indicates whether the hour element is locked.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean isHourElementLocked() {
		return false;
	}

	/**
	 * Indicates whether the day-of-month element is locked.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean isDayNumberElementLocked() {
		return false;
	}

	/**
	 * Indicates whether the month element is locked.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean isMonthElementLocked() {
		return false;
	}

	/**
	 * Indicates whether the day-of-week element is locked.
	 *
	 * @return {@code true}.
	 */
	@Override
	public boolean isDayOfWeekElementLocked() {
		return true;
	}
}
