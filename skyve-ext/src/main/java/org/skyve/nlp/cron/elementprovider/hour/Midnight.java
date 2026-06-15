package org.skyve.nlp.cron.elementprovider.hour;

import org.skyve.nlp.cron.ExpressionElementProvider;

/**
 * Detects the {@code midnight} keyword and contributes fixed hour/minute cron values.
 */
public class Midnight implements ExpressionElementProvider {

	/**
	 * Indicates whether the last parsed input contained a midnight reference.
	 */
	protected boolean match = false;

	/**
	 * Matches the {@code midnight} keyword.
	 *
	 * @param value The text to inspect.
	 * @return {@code true} when midnight is referenced.
	 */
	@Override
	public boolean matches(String value) {
		match = value != null && value.toLowerCase().indexOf("midnight") >= 0;
		return match;
	}

	/**
	 * Indicates whether this provider can supply a minute token.
	 *
	 * @return {@code true} when midnight was matched.
	 */
	@Override
	public boolean canProvideMinute() {
		return match;
	}

	/**
	 * Returns the minute token for midnight.
	 *
	 * @return {@code "0"}.
	 */
	@Override
	public String getMinuteElement() {
		return "0";
	}

	/**
	 * Indicates whether this provider can supply an hour token.
	 *
	 * @return {@code true} when midnight was matched.
	 */
	@Override
	public boolean canProvideHour() {
		return match;
	}

	/**
	 * Returns the hour token for midnight.
	 *
	 * @return {@code "0"}.
	 */
	@Override
	public String getHourElement() {
		return "0";
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
	 * Returns the day-of-month token.
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
	 * Returns the month token.
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
	 * Returns the day-of-week token.
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
	 * @return {@code true} when midnight was matched.
	 */
	@Override
	public boolean isMinuteElementLocked() {
		return canProvideMinute();
	}

	/**
	 * Indicates whether the hour token is locked.
	 *
	 * @return {@code true} when midnight was matched.
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
