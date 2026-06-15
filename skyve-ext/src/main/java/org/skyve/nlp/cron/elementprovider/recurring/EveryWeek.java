package org.skyve.nlp.cron.elementprovider.recurring;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

/**
 * Parses weekly recurrence phrases and contributes weekly cron fields.
 */
public class EveryWeek implements ExpressionElementProvider {

	/**
	 * Regex pattern for weekly recurrence phrases.
	 */
	private static final String PATTERN = "(weekly|(every|each)\\sweek)";
	/**
	 * Compiled matcher for weekly recurrence phrases.
	 */
	private Pattern compiledPattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);
	/**
	 * Captured regex groups from the most recent match operation.
	 */
	private List<String> segments = new ArrayList<>();

	/**
	 * Matches weekly recurrence language.
	 *
	 * @param value The text to inspect.
	 * @return {@code true} when weekly syntax is detected.
	 */
	@Override
	public boolean matches(String value) {
		Matcher m = compiledPattern.matcher(value);
		while (m.find()) {
			for (int i = 0; i <= m.groupCount(); i++) {
				segments.add(m.group(i));
			}
		}
		return ! segments.isEmpty();
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
	 * @return {@code true} when weekly recurrence was matched.
	 */
	@Override
	public boolean canProvideDayOfWeek() {
		return ! segments.isEmpty();
	}

	/**
	 * Returns the day-of-week element for weekly recurrence.
	 *
	 * @return {@code "0"}.
	 */
	@Override
	public String getDayOfWeekElement() {
		return "0";
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
