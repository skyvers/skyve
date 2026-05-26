package org.skyve.nlp.cron;

/**
 * Defines a parser fragment that extracts one or more cron fields from natural-language text.
 *
 * <p>Implementations are stateful during a single parse attempt and are expected to be used on
 * a per-parse basis by {@link NaturalCronExpressionParser}.
 */
public interface ExpressionElementProvider {
	/**
	 * Determines whether this provider can interpret any part of {@code value}.
	 *
	 * @param value The natural-language text to inspect.
	 * @return {@code true} when this provider captured matching segments.
	 */
	public boolean matches(String value);

	/**
	 * Indicates whether this provider can supply the cron second field.
	 *
	 * @return {@code true} when a second element is available.
	 */
	default boolean canProvideSecond() {
		return false;
	}

	/**
	 * Returns the cron second field contributed by this provider.
	 *
	 * @return A valid cron second token or {@code null} when not provided.
	 */
	default String getSecondElement() {
		return null;
	}

	/**
	 * Indicates whether this provider can supply the cron minute field.
	 *
	 * @return {@code true} when a minute element is available.
	 */
	public boolean canProvideMinute();

	/**
	 * Returns the cron minute field contributed by this provider.
	 *
	 * @return A valid cron minute token or {@code null} when this provider contributes no minute.
	 */
	public String getMinuteElement();

	/**
	 * Indicates whether this provider can supply the cron hour field.
	 *
	 * @return {@code true} when an hour element is available.
	 */
	public boolean canProvideHour();

	/**
	 * Returns the cron hour field contributed by this provider.
	 *
	 * @return A valid cron hour token or {@code null} when this provider contributes no hour.
	 */
	public String getHourElement();

	/**
	 * Indicates whether this provider can supply the cron day-of-month field.
	 *
	 * @return {@code true} when a day-of-month element is available.
	 */
	public boolean canProvideDayNumber();

	/**
	 * Returns the cron day-of-month field contributed by this provider.
	 *
	 * @return A valid cron day-of-month token or {@code null} when not provided.
	 */
	public String getDayNumberElement();

	/**
	 * Indicates whether this provider can supply the cron month field.
	 *
	 * @return {@code true} when a month element is available.
	 */
	public boolean canProvideMonth();

	/**
	 * Returns the cron month field contributed by this provider.
	 *
	 * @return A valid cron month token or {@code null} when not provided.
	 */
	public String getMonthElement();

	/**
	 * Indicates whether this provider can supply the cron day-of-week field.
	 *
	 * @return {@code true} when a day-of-week element is available.
	 */
	public boolean canProvideDayOfWeek();

	/**
	 * Returns the cron day-of-week field contributed by this provider.
	 *
	 * @return A valid cron day-of-week token or {@code null} when not provided.
	 */
	public String getDayOfWeekElement();

	/**
	 * Indicates whether the second field value is fixed and should not be overridden.
	 *
	 * @return {@code true} when the second element is locked.
	 */
	default boolean isSecondElementLocked() {
		return false;
	}

	/**
	 * Indicates whether the minute field value is fixed and should not be overridden.
	 *
	 * @return {@code true} when the minute element is locked.
	 */
	public boolean isMinuteElementLocked();

	/**
	 * Indicates whether the hour field value is fixed and should not be overridden.
	 *
	 * @return {@code true} when the hour element is locked.
	 */
	public boolean isHourElementLocked();

	/**
	 * Indicates whether the day-of-month field value is fixed and should not be overridden.
	 *
	 * @return {@code true} when the day-of-month element is locked.
	 */
	public boolean isDayNumberElementLocked();

	/**
	 * Indicates whether the month field value is fixed and should not be overridden.
	 *
	 * @return {@code true} when the month element is locked.
	 */
	public boolean isMonthElementLocked();

	/**
	 * Indicates whether the day-of-week field value is fixed and should not be overridden.
	 *
	 * @return {@code true} when the day-of-week element is locked.
	 */
	public boolean isDayOfWeekElementLocked();
}
