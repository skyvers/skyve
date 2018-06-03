package org.skyve.nlp.cron;

public interface ExpressionElementProvider {

	public boolean matches(String value);

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

	public boolean isMinuteElementLocked();

	public boolean isHourElementLocked();

	public boolean isDayNumberElementLocked();

	public boolean isMonthElementLocked();

	public boolean isDayOfWeekElementLocked();
}
