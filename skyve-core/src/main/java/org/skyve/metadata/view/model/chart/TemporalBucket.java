package org.skyve.metadata.view.model.chart;

import java.time.Month;
import java.time.format.TextStyle;

import org.skyve.CORE;

/**
 * Bucket a DateOnly, DateTime, TimeOnly or Timestamp (or any extension of java.util.Date)
 * by a TemporalBucketType.
 * 
 * @author mike
 */
public class TemporalBucket implements Bucket {
	public static enum TemporalBucketType {
		dayMonthYear, day, month, year, monthYear, hour, hourDay, hourDayMonth, minuteHour, secondMinuteHour
	}
	
	private TemporalBucketType type = null;
	
	public TemporalBucket(TemporalBucketType type) {
		this.type = type;
	}

	@Override
	public String bizQLExpression(String categoryBindingOrAlias) {
		StringBuilder result = new StringBuilder(128);
		if (TemporalBucketType.dayMonthYear.equals(type)) {
			result.append("concat(year(").append(categoryBindingOrAlias);
			result.append("), '-', month(").append(categoryBindingOrAlias);
			result.append("), '-', day(").append(categoryBindingOrAlias).append("))");
		}
		else if (TemporalBucketType.day.equals(type)) {
			result.append("day(").append(categoryBindingOrAlias).append(')');
		}
		else if (TemporalBucketType.month.equals(type)) {
			result.append("month(").append(categoryBindingOrAlias).append(')');
		}
		else if (TemporalBucketType.year.equals(type)) {
			result.append("year(").append(categoryBindingOrAlias).append(')');
		}
		else if (TemporalBucketType.monthYear.equals(type)) {
			result.append("concat(year(").append(categoryBindingOrAlias);
			result.append("), '-', month(").append(categoryBindingOrAlias).append("))");
		}
		else if (TemporalBucketType.hour.equals(type)) {
			result.append("hour(").append(categoryBindingOrAlias).append(')');
		}
		else if (TemporalBucketType.hourDay.equals(type)) {
			result.append("concat(day(").append(categoryBindingOrAlias);
			result.append("), ' ', hour(").append(categoryBindingOrAlias).append("))");
		}
		else if (TemporalBucketType.hourDayMonth.equals(type)) {
			result.append("concat(month(").append(categoryBindingOrAlias);
			result.append("), '-', day(").append(categoryBindingOrAlias);
			result.append("), ' ', hour(").append(categoryBindingOrAlias).append("))");
		}
		else if (TemporalBucketType.minuteHour.equals(type)) {
			result.append("concat(hour(").append(categoryBindingOrAlias);
			result.append("), ':', minute(").append(categoryBindingOrAlias).append("))");
		}
		else if (TemporalBucketType.secondMinuteHour.equals(type)) {
			result.append("concat(hour(").append(categoryBindingOrAlias);
			result.append("), ':', minute(").append(categoryBindingOrAlias);
			result.append("), ':', second(").append(categoryBindingOrAlias).append("))");
		}
		return result.toString();
	}

	@Override
	public String label(Object category) {
		StringBuilder result = new StringBuilder(16);
		if (category == null) {
			result.append("Unknown");
		}
		else if (TemporalBucketType.dayMonthYear.equals(type)) {
			String[] tokens = category.toString().split("-");
			result.append(day(Integer.parseInt(tokens[2]))).append(' ');
			result.append(month(Integer.parseInt(tokens[1]))).append(' ').append(tokens[0]);
		}
		else if (TemporalBucketType.day.equals(type)) {
			result.append(day(((Number) category).intValue()));
		}
		else if (TemporalBucketType.month.equals(type)) {
			result.append(month(((Number) category).intValue()));
		}
		else if (TemporalBucketType.year.equals(type)) {
			result.append(category);
		}
		else if (TemporalBucketType.monthYear.equals(type)) {
			String[] tokens = category.toString().split("-");
			result.append(month(Integer.parseInt(tokens[1]))).append(' ').append(tokens[0]);
		}
		else if (TemporalBucketType.hour.equals(type)) {
			result.append(padZero(category)).append(":00");
		}
		else if (TemporalBucketType.hourDay.equals(type)) {
			String[] tokens = category.toString().split("\\s");
			result.append(day(Integer.parseInt(tokens[0]))).append(' ').append(padZero(tokens[1])).append(":00");
		}
		else if (TemporalBucketType.hourDayMonth.equals(type)) {
			String[] tokens = category.toString().split("(\\s|-)");
			result.append(month(Integer.parseInt(tokens[0]))).append(' ');
			result.append(day(Integer.parseInt(tokens[1]))).append(' ').append(padZero(tokens[2])).append(":00");
		}
		else if (TemporalBucketType.minuteHour.equals(type)) {
			String[] tokens = category.toString().split(":");
			result.append(padZero(tokens[0])).append(':').append(padZero(tokens[1]));
		}
		else if (TemporalBucketType.secondMinuteHour.equals(type)) {
			String[] tokens = category.toString().split(":");
			result.append(padZero(tokens[0])).append(':');
			result.append(padZero(tokens[1])).append(':').append(padZero(tokens[2]));
		}
		return result.toString();
	}
	
	private static String day(int day) {
		if ((day == 1) || (day == 21) || (day == 31)) {
			return day + "st";
		}
		else if ((day == 2) || (day == 22)) {
			return day + "nd";
		}
		else if ((day == 3) || (day == 23)) {
			return day + "rd";
		}
		else {
			return day + "th";
		}
	}
	
	private static String month(int month) {
		return Month.of(month).getDisplayName(TextStyle.SHORT, CORE.getUser().getLocale());
	}
	
	private static String padZero(Object timeComponent) {
		String result = timeComponent.toString();
		if (result.length() == 1) {
			result = "0" + result;
		}
		return result;
	}
}
