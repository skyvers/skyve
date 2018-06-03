package org.skyve.nlp.cron.elementprovider.hour;

import org.skyve.nlp.cron.ExpressionElementProvider;

public class Midnight implements ExpressionElementProvider {

	protected boolean match = false;

	@Override
	public boolean matches(String value) {
		match = value != null && value.toLowerCase().indexOf("midnight") >= 0;
		return match;
	}

	@Override
	public boolean canProvideMinute() {
		return match;
	}

	@Override
	public String getMinuteElement() {
		return "0";
	}

	@Override
	public boolean canProvideHour() {
		return match;
	}

	@Override
	public String getHourElement() {
		return "0";
	}

	@Override
	public boolean canProvideDayNumber() {
		return false;
	}

	@Override
	public String getDayNumberElement() {
		return null;
	}

	@Override
	public boolean canProvideMonth() {
		return false;
	}

	@Override
	public String getMonthElement() {
		return null;
	}

	@Override
	public boolean canProvideDayOfWeek() {
		return false;
	}

	@Override
	public String getDayOfWeekElement() {
		return null;
	}

	@Override
	public boolean isMinuteElementLocked() {
		return canProvideMinute();
	}

	@Override
	public boolean isHourElementLocked() {
		return canProvideHour();
	}

	@Override
	public boolean isDayNumberElementLocked() {
		return false;
	}

	@Override
	public boolean isMonthElementLocked() {
		return false;
	}

	@Override
	public boolean isDayOfWeekElementLocked() {
		return false;
	}
}
