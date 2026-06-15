package org.skyve.nlp.cron.elementprovider.recurring;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

/**
 * Parses stepped day-of-month recurrence such as {@code every 3 days}.
 */
public class EveryDayNumberStep implements ExpressionElementProvider {

	/**
	 * Regex pattern for stepped day recurrence phrases.
	 */
	private static final String PATTERN = "(every\\s?(3[0-1]|2\\d|1[1-9]|[1-9])?\\s?days)";
	/**
	 * Compiled matcher for stepped day recurrence phrases.
	 */
	private Pattern compiledPattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);
	/**
	 * Captured regex groups from the most recent match operation.
	 */
	private List<String> segments = new ArrayList<>();

	/**
	 * Matches stepped day recurrence phrases.
	 *
	 * @param value The text to inspect.
	 * @return {@code true} when stepped day language is detected.
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
	 * Indicates whether this provider can supply a stepped day-of-month element.
	 *
	 * @return {@code true} when the expected day-step capture groups are available.
	 */
	@Override
	public boolean canProvideDayNumber() {
		return segments.size() == 3;
	}

	/**
	 * Returns the day-of-month cron token for stepped recurrence.
	 *
	 * @return A stepped token like {@code * / 3} (without spaces), or {@code *} when no explicit step is present.
	 */
	@Override
	public String getDayNumberElement() {
		return segments.size() > 2 ? String.format("*/%s", segments.get(2)) : "*";
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
