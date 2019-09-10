package org.skyve.nlp.cron;

public interface ExpressionElementProvider {

	public boolean matches(String value);

	default boolean canProvideSecond() {
		return false;
	}

	default String getSecondElement() {
		return null;
	}

	public boolean canProvideMinute();

	public String getMinuteElement();

	public boolean canProvideHour();

	public String getHourElement();

	public boolean canProvideDayNumber();

	public String getDayNumberElement();

	public boolean canProvideMonth();

	public String getMonthElement();

	public boolean canProvideDayOfWeek();

	public String getDayOfWeekElement();

	default boolean isSecondElementLocked() {
		return false;
	}

	public boolean isMinuteElementLocked();

	public boolean isHourElementLocked();

	public boolean isDayNumberElementLocked();

	public boolean isMonthElementLocked();

	public boolean isDayOfWeekElementLocked();
}
