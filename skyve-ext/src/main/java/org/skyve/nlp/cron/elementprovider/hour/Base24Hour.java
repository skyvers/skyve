package org.skyve.nlp.cron.elementprovider.hour;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

/**
 * Parses 24-hour clock values in {@code HH:mm} form and exposes cron field tokens.
 */
public class Base24Hour implements ExpressionElementProvider {

	/**
	 * Regex pattern for 24-hour time values in {@code HH:mm} form.
	 */
	private static final String PATTERN = "(2[0-3]|[01]?\\d):([0-5]?\\d)";
	/**
	 * Compiled form of the 24-hour time regex.
	 */
	private Pattern compiledPattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);

	/**
	 * Captured match groups from the latest parsed input.
	 */
	protected List<String> segments = new ArrayList<>();

	/**
	 * Matches and captures 24-hour time tokens from input text.
	 *
	 * @param value The natural-language text to inspect.
	 * @return {@code true} when one or more time tokens were captured.
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
		return ! segments.isEmpty();
	}

	/**
	 * Indicates whether a minute token can be provided.
	 *
	 * @return {@code true} when a minute component was captured.
	 */
	@Override
	public boolean canProvideMinute() {
		return segments.size() > 2;
	}

	/**
	 * Returns the parsed minute token for the cron expression.
	 *
	 * @return The minute token, or {@code null} when no minute component was matched.
	 */
	@Override
	public String getMinuteElement() {
		return segments.size() > 2 ? String.valueOf(Integer.parseInt(segments.get(2))) : null;
	}

	/**
	 * Indicates whether an hour token can be provided.
	 *
	 * @return {@code true} when an hour component was captured.
	 */
	@Override
	public boolean canProvideHour() {
		return segments.size() > 1;
	}

	/**
	 * Returns the parsed hour token for the cron expression.
	 *
	 * @return The hour token, or {@code null} when no hour component was matched.
	 */
	@Override
	public String getHourElement() {
		return segments.size() > 1 ? String.valueOf(Integer.parseInt(segments.get(1))) : null;
	}

	/**
	 * Indicates this provider does not supply a day-of-month token.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean canProvideDayNumber() {
		return false;
	}

	/**
	 * Returns the day-of-month token for this provider.
	 *
	 * @return {@code null}, as this provider does not supply day-of-month values.
	 */
	@Override
	public String getDayNumberElement() {
		return null;
	}

	/**
	 * Indicates this provider does not supply a month token.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean canProvideMonth() {
		return false;
	}

	/**
	 * Returns the month token for this provider.
	 *
	 * @return {@code null}, as this provider does not supply month values.
	 */
	@Override
	public String getMonthElement() {
		return null;
	}

	/**
	 * Indicates this provider does not supply a day-of-week token.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean canProvideDayOfWeek() {
		return false;
	}

	/**
	 * Returns the day-of-week token for this provider.
	 *
	 * @return {@code null}, as this provider does not supply day-of-week values.
	 */
	@Override
	public String getDayOfWeekElement() {
		return null;
	}

	/**
	 * Indicates whether the minute token is locked.
	 *
	 * @return {@code true} when a minute component has been captured.
	 */
	@Override
	public boolean isMinuteElementLocked() {
		return canProvideMinute();
	}

	/**
	 * Indicates whether the hour token is locked.
	 *
	 * @return {@code true} when an hour component has been captured.
	 */
	@Override
	public boolean isHourElementLocked() {
		return canProvideHour();
	}

	/**
	 * Indicates whether the day-of-month token is locked.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean isDayNumberElementLocked() {
		return false;
	}

	/**
	 * Indicates whether the month token is locked.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean isMonthElementLocked() {
		return false;
	}

	/**
	 * Indicates whether the day-of-week token is locked.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean isDayOfWeekElementLocked() {
		return false;
	}
}
