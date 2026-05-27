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
	 * Indicates whether this provider contributes the cron seconds field.
	 *
	 * @return {@code true} when {@link #getSecondElement()} is meaningful for the current parse
	 */
	default boolean canProvideSecond() {
		return false;
	}

	/**
	 * Returns the cron seconds token contributed by this provider.
	 *
	 * @return A cron seconds token, or {@code null} when no seconds value is provided
	 */
	default String getSecondElement() {
		return null;
	}

	/**
	 * Indicates whether this provider contributes the cron minute field.
	 *
	 * @return {@code true} when {@link #getMinuteElement()} is meaningful for the current parse
	 */
	public boolean canProvideMinute();

	/**
	 * Returns the cron minute field contributed by this provider.
	 *
	 * @return A valid cron minute token or {@code null} when this provider contributes no minute.
	 */
	public String getMinuteElement();

	/**
	 * Indicates whether this provider contributes the cron hour field.
	 *
	 * @return {@code true} when {@link #getHourElement()} is meaningful for the current parse
	 */
	public boolean canProvideHour();

	/**
	 * Returns the cron hour field contributed by this provider.
	 *
	 * @return A valid cron hour token or {@code null} when this provider contributes no hour.
	 */
	public String getHourElement();

	/**
	 * Indicates whether this provider contributes the cron day-of-month field.
	 *
	 * @return {@code true} when {@link #getDayNumberElement()} is meaningful for the current parse
	 */
	public boolean canProvideDayNumber();

	/**
	 * Returns the cron day-of-month field contributed by this provider.
	 *
	 * @return A valid cron day-of-month token or {@code null} when not provided.
	 */
	public String getDayNumberElement();

	/**
	 * Indicates whether this provider contributes the cron month field.
	 *
	 * @return {@code true} when {@link #getMonthElement()} is meaningful for the current parse
	 */
	public boolean canProvideMonth();

	/**
	 * Returns the cron month field contributed by this provider.
	 *
	 * @return A valid cron month token or {@code null} when not provided.
	 */
	public String getMonthElement();

	/**
	 * Indicates whether this provider contributes the cron day-of-week field.
	 *
	 * @return {@code true} when {@link #getDayOfWeekElement()} is meaningful for the current parse
	 */
	public boolean canProvideDayOfWeek();

	/**
	 * Returns the cron day-of-week field contributed by this provider.
	 *
	 * @return A valid cron day-of-week token or {@code null} when not provided.
	 */
	public String getDayOfWeekElement();

	/**
	 * Indicates whether the contributed seconds value is fixed and should not be overridden.
	 *
	 * @return {@code true} when the seconds token is immutable for this provider
	 */
	default boolean isSecondElementLocked() {
		return false;
	}

	/**
	 * Indicates whether the contributed minute value is fixed and should not be overridden.
	 *
	 * @return {@code true} when the minute token is immutable for this provider
	 */
	public boolean isMinuteElementLocked();

	/**
	 * Indicates whether the contributed hour value is fixed and should not be overridden.
	 *
	 * @return {@code true} when the hour token is immutable for this provider
	 */
	public boolean isHourElementLocked();

	/**
	 * Indicates whether the contributed day-of-month value is fixed and should not be overridden.
	 *
	 * @return {@code true} when the day-of-month token is immutable for this provider
	 */
	public boolean isDayNumberElementLocked();

	/**
	 * Indicates whether the contributed month value is fixed and should not be overridden.
	 *
	 * @return {@code true} when the month token is immutable for this provider
	 */
	public boolean isMonthElementLocked();

	/**
	 * Indicates whether the contributed day-of-week value is fixed and should not be overridden.
	 *
	 * @return {@code true} when the day-of-week token is immutable for this provider
	 */
	public boolean isDayOfWeekElementLocked();
}
