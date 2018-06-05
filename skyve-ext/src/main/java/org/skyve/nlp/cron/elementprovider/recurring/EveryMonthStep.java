package org.skyve.nlp.cron.elementprovider.recurring;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

public class EveryMonthStep implements ExpressionElementProvider {

	private static final String PATTERN = "(every\\s?(1[1-2]|[1-9])?\\s?months)";
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
		return "0";
	}

	@Override
	public boolean canProvideMonth() {
		return segments.size() == 3;
	}

	@Override
	public String getMonthElement() {
		return segments.size() > 2 ? String.format("*/%s", segments.get(2)) : "*";
	}

	@Override
	public boolean canProvideDayOfWeek() {
		return true;
	}

	@Override
	public String getDayOfWeekElement() {
		return "*";
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
		return true;
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
