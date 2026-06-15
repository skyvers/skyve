package org.skyve.nlp.cron.elementprovider.recurring;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

/**
 * Parses month-based recurrence phrases such as {@code monthly} and named months.
 */
public class EveryMonth implements ExpressionElementProvider {

	/**
	 * Regex pattern for monthly recurrence phrases.
	 */
	private static final String PATTERN = "(monthly|(every|each)\\s(month|january|february|march|april|may|june|july|august|september|october|november|december))";
	/**
	 * Compiled matcher for month recurrence phrases.
	 */
	private Pattern compiledPattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);
	/**
	 * Captured regex groups from the latest match operation.
	 */
	private List<String> segments = new ArrayList<>();
	/**
	 * Maps month names to cron month numbers.
	 */
	private Map<String, String> monthMap;

	/**
	 * Creates month-name mappings used to translate month words into cron month values.
	 */
	public EveryMonth() {
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
	 * Matches monthly recurrence language.
	 *
	 * @param value The text to inspect.
	 * @return {@code true} when a supported monthly pattern is found.
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
	 * Returns the default day-of-month element.
	 *
	 * @return {@code "1"}.
	 */
	@Override
	public String getDayNumberElement() {
		return "1";
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
	 * Returns the month cron token derived from the parsed phrase.
	 *
	 * @return {@code *} for generic monthly schedules, a numeric month token, or {@code null}.
	 */
	@Override
	public String getMonthElement() {
		if ((!segments.isEmpty() && "monthly".equalsIgnoreCase(segments.get(0)))
				|| (segments.size() > 3 && "month".equalsIgnoreCase(segments.get(3)))) {
			return "*";
		}
		return segments.size() > 3 && monthMap.get(segments.get(3).toLowerCase()) != null
				? monthMap.get(segments.get(3).toLowerCase())
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
	 * @return {@code false}.
	 */
	@Override
	public boolean isMonthElementLocked() {
		return false;
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
