package org.skyve.nlp.cron.elementprovider.recurring;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.skyve.nlp.cron.ExpressionElementProvider;

public class EveryDayNumber implements ExpressionElementProvider {

	private static final String PATTERN = "(every|each)\\s([0-9]?[0-9])(st|nd|rd|th)\\sof\\s(month|january|february|march|april|may|june|july|august|september|october|november|december)";
	private Pattern pattern = Pattern.compile(PATTERN, Pattern.CASE_INSENSITIVE);
	private List<String> segments = new ArrayList<>();
	private Map<String, String> monthMap;

	public EveryDayNumber() {
		monthMap = new HashMap<>();

		monthMap.put("january", "1");
		monthMap.put("february", "2");
		monthMap.put("march", "3");
		monthMap.put("april", "4");
		monthMap.put("may", "5");
		monthMap.put("june", "6");
		monthMap.put("july", "7");
		monthMap.put("august", "8");
		monthMap.put("september", "9");
		monthMap.put("october", "10");
		monthMap.put("november", "11");
		monthMap.put("december", "12");
	}

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
		return segments.size() >= 3 ? segments.get(2) : null;
	}

	@Override
	public boolean canProvideMonth() {
		return segments.size() >= 5;
	}

	@Override
	public String getMonthElement() {
		return segments.size() >= 5 && monthMap.get(segments.get(4).toLowerCase()) != null
				? monthMap.get(segments.get(4).toLowerCase())
				: null;
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
		return false;
	}

	@Override
	public boolean isMonthElementLocked() {
		return canProvideMonth();
	}

	@Override
	public boolean isDayOfWeekElementLocked() {
		return false;
	}
}
