package org.skyve.nlp.cron.elementprovider.recurring;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

/**
 * Parses phrases like {@code every 15th of march} into day-of-month and month cron fields.
 */
public class EveryDayNumber implements ExpressionElementProvider {

	/**
	 * Regex pattern for "every Nth of month" recurrence expressions.
	 */
	private static final String PATTERN = "(every|each)\\s(\\d?\\d)(st|nd|rd|th)\\sof\\s(month|january|february|march|april|may|june|july|august|september|october|november|december)";
	/**
	 * Compiled matcher for ordinal day-in-month expressions.
	 */
	private Pattern compiledPattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);
	/**
	 * Captured regex groups from the latest match operation.
	 */
	private List<String> segments = new ArrayList<>();
	/**
	 * Maps month names to cron month numeric values.
	 */
	private Map<String, String> monthMap;

	/**
	 * Creates month-name mappings used to resolve month words into cron month numbers.
	 */
	public EveryDayNumber() {
		monthMap = new HashMap<>();

		monthMap.put("january", "1");
		monthMap.put("february", "2");
		monthMap.put("march", "3");
		monthMap.put("april", "4");
		monthMap.put("may", "5");
		monthMap.put("june", "6");
		monthMap.put("july", "7");
		monthMap.put("august", "8");
		monthMap.put("september", "9");
		monthMap.put("october", "10");
		monthMap.put("november", "11");
		monthMap.put("december", "12");
	}

	/**
	 * Matches ordinal day-in-month language.
	 *
	 * @param value The text to inspect.
	 * @return {@code true} when an ordinal day and month are captured.
	 */
	@Override
	public boolean matches(String value) {
		Matcher m = compiledPattern.matcher(value);
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
	 * Returns the parsed day-of-month token.
	 *
	 * @return The day token, or {@code null} when unavailable.
	 */
	@Override
	public String getDayNumberElement() {
		return segments.size() >= 3 ? segments.get(2) : null;
	}

	/**
	 * Indicates whether this provider can supply a month element.
	 *
	 * @return {@code true} when the month capture group is available.
	 */
	@Override
	public boolean canProvideMonth() {
		return segments.size() >= 5;
	}

	/**
	 * Returns the parsed month token.
	 *
	 * @return The numeric month token, or {@code null} when unavailable.
	 */
	@Override
	public String getMonthElement() {
		return segments.size() >= 5 && monthMap.get(segments.get(4).toLowerCase()) != null
				? monthMap.get(segments.get(4).toLowerCase())
				: null;
	}

	/**
	 * Indicates this provider can supply a day-of-week element.
	 *
	 * @return {@code true}.
	 */
	@Override
	public boolean canProvideDayOfWeek() {
		return true;
	}

	/**
	 * Returns the day-of-week wildcard element.
	 *
	 * @return {@code "*"}.
	 */
	@Override
	public String getDayOfWeekElement() {
			return "*";
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
	 * @return {@code true} when a month token can be provided.
	 */
	@Override
	public boolean isMonthElementLocked() {
		return canProvideMonth();
	}

	/**
	 * Indicates whether the day-of-week element is locked.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean isDayOfWeekElementLocked() {
		return false;
	}
}
