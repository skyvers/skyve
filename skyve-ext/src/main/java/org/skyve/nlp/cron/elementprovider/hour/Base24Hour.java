package org.skyve.nlp.cron.elementprovider.hour;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

public class Base24Hour implements ExpressionElementProvider {

	private static final String PATTERN = "(2[0-3]|[01]?[0-9]):([0-5]?[0-9])";
	private Pattern pattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);

	protected List<String> segments = new ArrayList<>();

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
		return segments.size() > 2;
	}

	@Override
	public String getMinuteElement() {
		return segments.size() > 2 ? String.valueOf(Integer.parseInt(segments.get(2))) : null;
	}

	@Override
	public boolean canProvideHour() {
		return segments.size() > 1;
	}

	@Override
	public String getHourElement() {
		return segments.size() > 1 ? String.valueOf(Integer.parseInt(segments.get(1))) : null;
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
