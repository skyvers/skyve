package org.skyve.nlp.cron;

import java.text.SimpleDateFormat;
import java.util.regex.Pattern;

import com.ibm.icu.util.Calendar;

public class CronExpression {

	public String minute = null;
	public String hour = null;
	public String dayNumber = null;
	public String month = null;
	public String dayOfWeek = null;

	CronExpression() {
		// default constructor
	}

	public CronExpression(String minute, String hour, String dayNumber, String month, String dayOfWeek) {
		super();
		this.minute = minute;
		this.hour = hour;
		this.dayNumber = dayNumber;
		this.month = month;
		this.dayOfWeek = dayOfWeek;
	}

	public CronExpression(Integer minute, Integer hour, Integer dayNumber, Integer month, Integer dayOfWeek) {
		super();
		this.minute = minute != null ? String.valueOf(minute) : null;
		this.hour = hour != null ? String.valueOf(hour) : null;
		this.dayNumber = dayNumber != null ? String.valueOf(dayNumber) : null;
		this.month = month != null ? String.valueOf(month) : null;
		this.dayOfWeek = dayOfWeek != null ? String.valueOf(dayOfWeek) : null;
	}

	/**
	 * Returns a CronExpression for the specified cron expression string.
	 * 
	 * @param expression A cron string
	 * @return A CronExpression
	 */
	public static CronExpression fromExpression(final String expression) {
		if (expression != null) {
			String[] parts = expression.split("\\s");
			if (parts.length != 5) {
				throw new CronParserException("Unexpected expression, expected 5 parts, e.g. * * * * *.");
			}

			CronExpression ce = new CronExpression();
			ce.setMinute(parts[0]);
			ce.setHour(parts[1]);
			ce.setDayNumber(parts[2]);
			ce.setMonth(parts[3]);
			ce.setDayOfWeek(parts[4]);

			return ce;
		}

		return null;
	}

	public String getMinute() {
		return minute;
	}

	public boolean hasMinute() {
		return getMinute() != null;
	}

	public CronExpression setMinute(String minute) {
		this.minute = minute;
		return this;
	}

	public CronExpression setMinute(Integer minute) {
		return setMinute(String.valueOf(minute));
	}

	public String getHour() {
		return hour;
	}

	public boolean hasHour() {
		return getHour() != null;
	}

	public CronExpression setHour(String hour) {
		this.hour = hour;
		return this;
	}

	public CronExpression setHour(Integer minute) {
		return setHour(String.valueOf(minute));
	}

	public String getDayNumber() {
		return dayNumber;
	}

	public boolean hasDayNumber() {
		return getDayNumber() != null;
	}

	public CronExpression setDayNumber(String dayNumber) {
		this.dayNumber = dayNumber;
		return this;
	}

	public CronExpression setDayNumber(Integer minute) {
		return setDayNumber(String.valueOf(minute));
	}

	public String getMonth() {
		return month;
	}

	public boolean hasMonth() {
		return getMonth() != null;
	}

	public CronExpression setMonth(String month) {
		this.month = month;
		return this;
	}

	public CronExpression setMonth(Integer minute) {
		return setMonth(String.valueOf(minute));
	}

	public String getDayOfWeek() {
		return dayOfWeek;
	}

	public boolean hasDayOfWeek() {
		return getDayOfWeek() != null;
	}

	public CronExpression setDayOfWeek(String dayOfWeek) {
		this.dayOfWeek = dayOfWeek;
		return this;
	}

	public CronExpression setDayOfWeek(Integer minute) {
		return setDayOfWeek(String.valueOf(minute));
	}

	public boolean hasNothing() {
		return !hasMinute() && !hasHour() && !hasDayNumber() && !hasMonth() && !hasDayOfWeek();
	}

	@SuppressWarnings("boxing")
	public String toNaturalLanguage() {
		if (!hasNothing()) {
			// check if it's one of our static expressions
			String expression = toString();
			if (expression.equals("0 0 1 1 *"))
				return "yearly";
			if (expression.equals("0 0 1 * *"))
				return "monthly";
			if (expression.equals("0 0 * * 0"))
				return "weekly";
			if (expression.equals("0 0 * * *"))
				return "daily";
			if (expression.equals("0 0 * * *"))
				return "midnight";
			if (expression.equals("0 12 * * *"))
				return "midday";
			if (expression.equals("* * * * *"))
				return "every minute";
			if (expression.equals("0 * * * *"))
				return "hourly";
			if (expression.equals("0 0 * * *"))
				return "daily";

			StringBuilder b = new StringBuilder();
			b.append("every ");

			Pattern timeSetPattern = Pattern.compile("^([1-9]|[0-5][0-9])$");
			Pattern timeStepSetPattern = Pattern.compile("^\\*\\/([1-9]|[0-5][0-9])$");

			// minutes
			if (timeSetPattern.matcher(getMinute()).matches()) {
				if (getHour().equals("*")) {
					b.append(getMinute()).append(" past the hour");
				}
				// let hour capture the minute if set
			}

			if (timeStepSetPattern.matcher(getMinute()).matches()) {
				if (getHour().equals("*")) {
					b.append(getMinute().replaceAll("[^0-9]", "")).append(" minutes");
				}
			}

			// hours
			if (timeSetPattern.matcher(getHour()).matches()) {
				Integer h = Integer.valueOf(getHour());

				if (getMinute().equals("*")) {
					b.append("minute past ")
							.append(h >= 12 ? h - 12 : h)
							.append(h >= 12 ? "pm" : "am");
				} else if (timeStepSetPattern.matcher(getMinute()).matches()) {
					Integer m = Integer.valueOf(getMinute().replaceAll("[^0-9]", ""));
					b.append(m)
							.append(" minutes past ")
							.append(h >= 12 ? h - 12 : h)
							.append(h >= 12 ? "pm" : "am");
				} else if (timeSetPattern.matcher(getMinute()).matches()) {
					Integer m = Integer.valueOf(getMinute().replaceAll("[^0-9]", ""));
					if (getDayNumber().equals("*")) {
						b.append("day at ");
					}
					b.append(h >= 12 ? h - 12 : h)
							.append(":")
							.append(m < 10 ? "0" + m : m)
							.append(h >= 12 ? "pm" : "am");
				} else if (getMinute().equals("0")) {
					if (getDayNumber().equals("*")) {
						b.append("day at ");
					}
					b.append(h >= 12 ? h - 12 : h)
							.append(h >= 12 ? "pm" : "am");
				}
			}

			if (timeStepSetPattern.matcher(getHour()).matches()) {
				b.append(getHour().replaceAll("[^0-9]", "")).append(" hours");
			}

			// day of month
			if (!getDayNumber().equals("*")) {
				Pattern dayOfMonthSetPattern = Pattern.compile("^([1-9]|[1-2][0-9]|3[0-1])$");
				if (dayOfMonthSetPattern.matcher(getDayNumber()).matches()) {
					b.append(String.format("%s%s", getDayNumber(), getDayNumberSuffix(Integer.valueOf(getDayNumber()))));
				}
			}

			// month
			if (!getMonth().equals("*")) {
				Pattern monthSetPattern = Pattern.compile("^([1-9]|1[0-2])$");
				if (monthSetPattern.matcher(getMonth()).matches()) {
					Calendar c = Calendar.getInstance();
					c.set(Calendar.MONTH, Integer.valueOf(getMonth()) - 1);
					SimpleDateFormat sdf = new SimpleDateFormat("MMMM");
					if(!getDayNumber().equals("*")) {
						b.append(" of ");
					}
					b.append(sdf.format(c.getTime()));
				}
			}

			// day of week
			Pattern daySetPattern = Pattern.compile("[0-6]");
			if (daySetPattern.matcher(getDayOfWeek()).matches()) {
				Calendar c = Calendar.getInstance();
				c.set(Calendar.DAY_OF_WEEK, Integer.valueOf(getDayOfWeek()) + 1);
				SimpleDateFormat sdf = new SimpleDateFormat("EEEE");

				if (b.toString().endsWith("minutes")) {
					b.append(" on ")
							.append(sdf.format(c.getTime()))
							.append("s");
				} else {
					b.append(sdf.format(c.getTime()));
				}
			}

			return b.toString();
		}

		return null;
	}

	@Override
	@SuppressWarnings("boxing")
	public String toString() {
		return String.format("%s %s %s %s %s",
				hasMinute() ? getMinute() : 0,
				hasHour() ? getHour() : 0,
				hasDayNumber() ? getDayNumber() : '*',
				hasMonth() ? getMonth() : '*',
				hasDayOfWeek() ? getDayOfWeek() : '*');
	}

	/**
	 * Return a valid suffix for a positional number
	 * 
	 * @param day
	 * @return
	 */
	private static String getDayNumberSuffix(int day) {
		if (day >= 11 && day <= 13) {
			return "th";
		}
		switch (day % 10) {
			case 1:
				return "st";
			case 2:
				return "nd";
			case 3:
				return "rd";
			default:
				return "th";
		}
	}
}
