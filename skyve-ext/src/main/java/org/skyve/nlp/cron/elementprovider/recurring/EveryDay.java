package org.skyve.nlp.cron.elementprovider.recurring;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

public class EveryDay implements ExpressionElementProvider {

	private static final String DAY_OF_WEEK_PATTERN = "(daily|(every|each|on)\\s(day|monday|tuesday|wednesday|thursday|friday|saturday|sunday)(?s))";

	private Pattern dayOfWeekPattern = Pattern.compile(DAY_OF_WEEK_PATTERN, Pattern.CASE_INSENSITIVE);

	private List<String> segments = new ArrayList<>();
	private Map<String, String> dayMap;

	public EveryDay() {
		dayMap = new HashMap<>();

		dayMap.put("sunday", "0");
		dayMap.put("monday", "1");
		dayMap.put("tuesday", "2");
		dayMap.put("wednesday", "3");
		dayMap.put("thursday", "4");
		dayMap.put("friday", "5");
		dayMap.put("saturday", "6");
	}

	@Override
	public boolean matches(String value) {
		Matcher m = dayOfWeekPattern.matcher(value);
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
		if ((segments.size() > 0 && segments.get(0).equals("daily")) || (segments.size() > 3 && segments.get(3).equals("day"))) {
			return "*";
		}
		return segments.size() > 3 && dayMap.get(segments.get(3).toLowerCase()) != null ? dayMap.get(segments.get(3).toLowerCase())
				: null;
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
		return true;
	}
}
