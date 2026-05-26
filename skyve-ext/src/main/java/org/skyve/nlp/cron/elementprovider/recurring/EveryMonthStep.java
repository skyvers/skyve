package org.skyve.nlp.cron.elementprovider.recurring;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

/**
 * Parses stepped month recurrence such as {@code every 2 months}.
 */
public class EveryMonthStep implements ExpressionElementProvider {

	/**
	 * Regex pattern for stepped month recurrence phrases.
	 */
	private static final String PATTERN = "(every\\s?(1[1-2]|[1-9])?\\s?months)";
	/**
	 * Compiled matcher for stepped month recurrence phrases.
	 */
	private Pattern compiledPattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);
	/**
	 * Captured regex groups from the most recent match operation.
	 */
	private List<String> segments = new ArrayList<>();

	/**
	 * Matches stepped monthly recurrence phrases.
	 *
	 * @param value The text to inspect.
	 * @return {@code true} when stepped month language is detected.
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
	 * Returns the day-of-month element used with stepped month schedules.
	 *
	 * @return {@code "0"}.
	 */
	@Override
	public String getDayNumberElement() {
		return "0";
	}

	/**
	 * Indicates whether this provider can supply a stepped month element.
	 *
	 * @return {@code true} when the expected capture groups are available.
	 */
	@Override
	public boolean canProvideMonth() {
		return segments.size() == 3;
	}

	/**
	 * Returns the month element for stepped recurrence.
	 *
	 * @return A stepped token in slash-step form (for example, star slash 2), or {@code *} when no explicit step is present.
	 */
	@Override
	public String getMonthElement() {
		return segments.size() > 2 ? String.format("*/%s", segments.get(2)) : "*";
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
	 * @return {@code true}.
	 */
	@Override
	public boolean isDayNumberElementLocked() {
		return true;
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
