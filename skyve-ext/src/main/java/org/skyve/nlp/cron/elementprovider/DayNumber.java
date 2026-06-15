package org.skyve.nlp.cron.elementprovider;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

/**
 * Extracts ordinal day-of-month phrases such as {@code 1st}, {@code 2nd}, and {@code 31st}.
 */
public class DayNumber implements ExpressionElementProvider {
	/**
	 * Regex pattern for ordinal day-of-month tokens.
	 */
	private static final String PATTERN = "(\\d?\\d)(st|nd|rd|th)";

	/**
	 * Compiled ordinal day pattern matcher.
	 */
	private Pattern compiledPattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);
	
	/**
	 * Captured regex groups from the most recent match operation.
	 */
	private List<String> segments = new ArrayList<>();

	/**
	 * Captures ordinal day tokens from natural-language input.
	 *
	 * @param value The text to parse.
	 * @return {@code true} when at least one ordinal day token was found.
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
	 * Returns the default minute element for this provider.
	 *
	 * @return {@code "0"}.
	 */
	@Override
	public String getMinuteElement() {
		return "0";
	}

	/**
	 * Indicates the minute element is not locked.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean isMinuteElementLocked() {
		return false;
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
	 * Returns the default hour element for this provider.
	 *
	 * @return {@code "0"}.
	 */
	@Override
	public String getHourElement() {
		return "0";
	}

	/**
	 * Indicates the hour element is not locked.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean isHourElementLocked() {
		return false;
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
	 * Returns the captured day-of-month element.
	 *
	 * @return The numeric day token, or {@code null} when no day was captured.
	 */
	@Override
	public String getDayNumberElement() {
		return segments.isEmpty() ? null : segments.get(1);
	}

	/**
	 * Indicates the day-of-month element is locked once detected.
	 *
	 * @return {@code true}.
	 */
	@Override
	public boolean isDayNumberElementLocked() {
		return true;
	}

	/**
	 * Indicates this provider can supply a month element when day tokens were matched.
	 *
	 * @return {@code true} when at least one day token has been captured.
	 */
	@Override
	public boolean canProvideMonth() {
		return !segments.isEmpty();
	}

	/**
	 * Returns the month element wildcard.
	 *
	 * @return {@code "*"}.
	 */
	@Override
	public String getMonthElement() {
		return "*";
	}

	/**
	 * Indicates the month element is not locked.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean isMonthElementLocked() {
		return false;
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
	 * Returns the day-of-week element wildcard.
	 *
	 * @return {@code "*"}.
	 */
	@Override
	public String getDayOfWeekElement() {
		return "*";
	}

	/**
	 * Indicates the day-of-week element is not locked.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean isDayOfWeekElementLocked() {
		return false;
	}
}
