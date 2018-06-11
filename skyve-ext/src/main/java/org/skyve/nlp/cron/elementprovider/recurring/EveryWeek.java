package org.skyve.nlp.cron.elementprovider.recurring;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

public class EveryWeek implements ExpressionElementProvider {

	private static final String PATTERN = "(weekly|(every|each)\\sweek)";
	private Pattern pattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);
	private List<String> segments = new ArrayList<>();

	@Override
	public boolean matches(String value) {
		Matcher m = pattern.matcher(value);
		while (m.find()) {
			for (int i = 0; i <= m.groupCount(); i++) {
				segments.add(m.group(i));
			}
		}
		return segments.size() > 0;
	}

	@Override
	public boolean canProvideMinute() {
		return true;
	}

	@Override
	public String getMinuteElement() {
		return "0";
	}

	@Override
	public boolean canProvideHour() {
		return true;
	}

	@Override
	public String getHourElement() {
		return "0";
	}

	@Override
	public boolean canProvideDayNumber() {
		return true;
	}

	@Override
	public String getDayNumberElement() {
		return "*";
	}

	@Override
	public boolean canProvideMonth() {
		return true;
	}

	@Override
	public String getMonthElement() {
		return "*";
	}

	@Override
	public boolean canProvideDayOfWeek() {
		return segments.size() > 0;
	}

	@Override
	public String getDayOfWeekElement() {
		return "0";
	}

	@Override
	public boolean isMinuteElementLocked() {
		return false;
	}

	@Override
	public boolean isHourElementLocked() {
		return false;
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
