package org.skyve.nlp.cron.elementprovider.recurring;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

/**
 * Parses recurring minute phrases such as {@code every minute} and {@code every 5 minute}.
 */
public class EveryMinute implements ExpressionElementProvider {

	/**
	 * Regex pattern for recurring-minute phrases.
	 */
	private static final String PATTERN = "((every|each) ?(\\d+)?\\sminute)";

	/**
	 * Compiled matcher for recurring-minute phrases.
	 */
	private Pattern compiledPattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);

	/**
	 * Captured regex groups from the most recent match operation.
	 */
	private List<String> segments = new ArrayList<>();

	/**
	 * Matches recurring-minute language in the supplied text.
	 *
	 * @param value The text to inspect.
	 * @return {@code true} when recurring-minute syntax is detected.
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
	 * @return {@code true} when a recurring-minute phrase was matched.
	 */
	@Override
	public boolean canProvideMinute() {
		return !segments.isEmpty();
	}

	/**
	 * Returns either wildcard minutes or step minutes depending on the parsed phrase.
	 *
	 * @return {@code *} or a stepped minute token like {@code * / n} (without spaces), or {@code null} when unsupported.
	 */
	@Override
	public String getMinuteElement() {
		if (canProvideMinute()) {
			return segments.size() > 3 ? String.format("*/%s", segments.get(3)) : "*";
		}
		return null;
	}

	/**
	 * Indicates whether this provider can supply an hour element.
	 *
	 * @return {@code true} when a recurring-minute phrase was matched.
	 */
	@Override
	public boolean canProvideHour() {
		return canProvideMinute();
	}

	/**
	 * Returns the hour wildcard element.
	 *
	 * @return {@code "*"}.
	 */
	@Override
	public String getHourElement() {
		return "*";
	}

	/**
	 * Indicates this provider does not supply a day-of-month element.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean canProvideDayNumber() {
		return false;
	}

	/**
	 * Returns the day-of-month element.
	 *
	 * @return {@code null}, as day-of-month values are not provided.
	 */
	@Override
	public String getDayNumberElement() {
		return null;
	}

	/**
	 * Indicates this provider does not supply a month element.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean canProvideMonth() {
		return false;
	}

	/**
	 * Returns the month element.
	 *
	 * @return {@code null}, as month values are not provided.
	 */
	@Override
	public String getMonthElement() {
		return null;
	}

	/**
	 * Indicates this provider does not supply a day-of-week element.
	 *
	 * @return {@code false}.
	 */
	@Override
	public boolean canProvideDayOfWeek() {
		return false;
	}

	/**
	 * Returns the day-of-week element.
	 *
	 * @return {@code null}, as day-of-week values are not provided.
	 */
	@Override
	public String getDayOfWeekElement() {
		return null;
	}

	/**
	 * Indicates whether the minute element is locked.
	 *
	 * @return {@code true}.
	 */
	@Override
	public boolean isMinuteElementLocked() {
		return true;
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
