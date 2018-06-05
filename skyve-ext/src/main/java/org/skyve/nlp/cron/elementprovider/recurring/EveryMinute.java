package org.skyve.nlp.cron.elementprovider.recurring;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

public class EveryMinute implements ExpressionElementProvider {

	private static final String PATTERN = "((every|each) ?([0-9]+)?\\sminute)";
	private Pattern pattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);
	private List<String> segments = new ArrayList<>();

	@Override
	public boolean matches(String value) {
		Matcher m = pattern.matcher(value);
		while (m.find()) {
			for (int i = 0; i <= m.groupCount(); i++) {
				if (m.group(i) != null) {
					segments.add(m.group(i));
				}
			}
		}
		return segments.size() > 0;
	}

	@Override
	public boolean canProvideMinute() {
		return segments.size() > 0;
	}

	@Override
	public String getMinuteElement() {
		if (canProvideMinute()) {
			return segments.size() > 3 ? String.format("*/%s", segments.get(3)) : "*";
		}
		return null;
	}

	@Override
	public boolean canProvideHour() {
		return canProvideMinute();
	}

	@Override
	public String getHourElement() {
		return "*";
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
		return true;
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
