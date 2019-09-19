package org.skyve.metadata.view.model.chart;

import java.time.Month;
import java.time.format.TextStyle;

import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.CORE;
import org.skyve.impl.util.XMLMetaData;

/**
 * Bucket a DateOnly, DateTime, TimeOnly or Timestamp (or any extension of java.util.Date)
 * by a TemporalBucketType.
 * 
 * @author mike
 */
@XmlTransient
public class TemporalBucket implements Bucket {
	private static final long serialVersionUID = -9134123560010408219L;

	@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
	public static enum TemporalBucketType {
		dayMonthYear, day, month, year, monthYear, hour, hourDay, hourDayMonth, minuteHour, secondMinuteHour
	}
	
	protected TemporalBucketType type = null;
	
	public TemporalBucket(TemporalBucketType type) {
		this.type = type;
	}

	@Override
	public String bizQLExpression(String categoryBindingOrAlias) {
		StringBuilder result = new StringBuilder(128);
		if (TemporalBucketType.dayMonthYear.equals(type)) {
			result.append("month(").append(categoryBindingOrAlias).append(')');
			padZero(result);
			String month = result.toString();
			
			result.setLength(0);
			result.append("day(").append(categoryBindingOrAlias).append(')');
			padZero(result);
			String day = result.toString();
			
			result.setLength(0);
			result.append("concat(cast(year(").append(categoryBindingOrAlias);
			result.append(") as string), '-', ").append(month);
			result.append(", '-', ").append(day).append(')');
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
			result.append("month(").append(categoryBindingOrAlias).append(')');
			padZero(result);
			String month = result.toString();
			
			result.setLength(0);
			result.append("concat(cast(year(").append(categoryBindingOrAlias);
			result.append(") as string), '-', ").append(month).append(')');
		}
		else if (TemporalBucketType.hour.equals(type)) {
			result.append("hour(").append(categoryBindingOrAlias).append(')');
			padZero(result);
		}
		else if (TemporalBucketType.hourDay.equals(type)) {
			result.append("day(").append(categoryBindingOrAlias).append(')');
			padZero(result);
			String day = result.toString();
			
			result.setLength(0);
			result.append("hour(").append(categoryBindingOrAlias).append(')');
			padZero(result);
			String hour = result.toString();
			
			result.setLength(0);
			result.append("concat(").append(day).append(", ' ', ").append(hour).append(')');
		}
		else if (TemporalBucketType.hourDayMonth.equals(type)) {
			result.append("month(").append(categoryBindingOrAlias).append(')');
			padZero(result);
			String month = result.toString();

			result.setLength(0);
			result.append("day(").append(categoryBindingOrAlias).append(')');
			padZero(result);
			String day = result.toString();

			result.setLength(0);
			result.append("hour(").append(categoryBindingOrAlias).append(')');
			padZero(result);
			String hour = result.toString();

			result.setLength(0);
			result.append("concat(").append(month);
			result.append(", '-', ").append(day);
			result.append(", ' ', ").append(hour).append(')');
		}
		else if (TemporalBucketType.minuteHour.equals(type)) {
			result.append("hour(").append(categoryBindingOrAlias).append(')');
			padZero(result);
			String hour = result.toString();

			result.setLength(0);
			result.append("minute(").append(categoryBindingOrAlias).append(')');
			padZero(result);
			String minute = result.toString();

			result.setLength(0);
			result.append("concat(").append(hour).append(", ':', ").append(minute).append(')');
		}
		else if (TemporalBucketType.secondMinuteHour.equals(type)) {
			result.append("hour(").append(categoryBindingOrAlias).append(')');
			padZero(result);
			String hour = result.toString();

			result.setLength(0);
			result.append("minute(").append(categoryBindingOrAlias).append(')');
			padZero(result);
			String minute = result.toString();

			result.setLength(0);
			result.append("second(").append(categoryBindingOrAlias).append(')');
			padZero(result);
			String second = result.toString();

			result.setLength(0);
			result.append("concat(").append(hour).append(", ':', ");
			result.append(minute).append(", ':', ").append(second).append(')');
		}
		return result.toString();
	}

	@Override
	public String label(Object category) {
		if (category == null) {
			return null;
		}
		
		StringBuilder result = new StringBuilder(16);
		if (TemporalBucketType.dayMonthYear.equals(type)) {
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
			result.append(category).append(":00");
		}
		else if (TemporalBucketType.hourDay.equals(type)) {
			String[] tokens = category.toString().split("\\s");
			result.append(day(Integer.parseInt(tokens[0]))).append(' ').append(tokens[1]).append(":00");
		}
		else if (TemporalBucketType.hourDayMonth.equals(type)) {
			String[] tokens = category.toString().split("(\\s|-)");
			result.append(month(Integer.parseInt(tokens[0]))).append(' ');
			result.append(day(Integer.parseInt(tokens[1]))).append(' ').append(tokens[2]).append(":00");
		}
		else if (TemporalBucketType.minuteHour.equals(type)) {
			String[] tokens = category.toString().split(":");
			result.append(tokens[0]).append(':').append(tokens[1]);
		}
		else if (TemporalBucketType.secondMinuteHour.equals(type)) {
			String[] tokens = category.toString().split(":");
			result.append(tokens[0]).append(':').append(tokens[1]).append(':').append(tokens[2]);
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
	
	private static void padZero(StringBuilder expression) {
		String string = expression.toString();
		expression.insert(0, "case when ");
		expression.append(" < 10 then concat('0', cast(").append(string);
		expression.append(" as string)) else concat('', cast(").append(string).append(" as string)) end");
	}
}
