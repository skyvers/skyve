package org.skyve.nlp.cron;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.regex.Pattern;

import org.skyve.CORE;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Represents a six-field cron expression and provides conversion helpers to and from
 * natural-language phrases.
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate cron grammar tokens.
public class CronExpression {
	public String second = null;
	public String minute = null;
	public String hour = null;
	public String dayNumber = null;
	public String month = null;
	public String dayOfWeek = null;

	CronExpression() {
		// default constructor
	}

	public CronExpression(@Nullable String second, @Nullable String minute, @Nullable String hour, @Nullable String dayNumber, @Nullable String month, @Nullable String dayOfWeek) {
		super();
		this.second = second;
		this.minute = minute;
		this.hour = hour;
		this.dayNumber = dayNumber;
		this.month = month;
		this.dayOfWeek = dayOfWeek;
	}

	public CronExpression(@Nullable Integer second, @Nullable Integer minute, @Nullable Integer hour, @Nullable Integer dayNumber, @Nullable Integer month, @Nullable Integer dayOfWeek) {
		super();
		this.second = second != null ? String.valueOf(second) : null;
		this.minute = minute != null ? String.valueOf(minute) : null;
		this.hour = hour != null ? String.valueOf(hour) : null;
		this.dayNumber = dayNumber != null ? String.valueOf(dayNumber) : null;
		this.month = month != null ? String.valueOf(month) : null;
		this.dayOfWeek = dayOfWeek != null ? String.valueOf(dayOfWeek) : null;
	}

	/**
	 * Parses a raw six-field cron string into a {@link CronExpression} instance.
	 *
	 * @param expression The cron text in {@code second minute hour day month dayOfWeek} format.
	 * @return The parsed expression, or {@code null} when {@code expression} is {@code null}.
	 * @throws CronParserException If the text does not contain exactly six fields.
	 */
	public static @Nullable CronExpression fromExpression(final @Nullable String expression) {
		if (expression != null) {
			String[] parts = expression.split("\\s");
			if (parts.length != 6) {
				throw new CronParserException("Unexpected expression, expected 5 parts, e.g. * * * * * *.");
			}

			CronExpression ce = new CronExpression();
			ce.setSecond(parts[0]);
			ce.setMinute(parts[1]);
			ce.setHour(parts[2]);
			ce.setDayNumber(parts[3]);
			ce.setMonth(parts[4]);
			ce.setDayOfWeek(parts[5]);

			return ce;
		}

		return null;
	}

	/**
	 * Returns the second.
	 */
	public @Nullable String getSecond() {
		return second;
	}

	/**
	 * Indicates whether hasSecond is satisfied.
	 */
	public boolean hasSecond() {
		return getSecond() != null;
	}

	/**
	 * Sets the second.
	 *
	 * @return This expression for fluent configuration.
	 */
	public @Nonnull CronExpression setSecond(@Nullable String second) {
		this.second = second;
		return this;
	}

	/**
	 * Sets the second.
	 *
	 * @return This expression for fluent configuration.
	 */
	public @Nonnull CronExpression setSecond(@Nonnull Integer second) {
		return setSecond(String.valueOf(second));
	}

	/**
	 * Returns the minute.
	 */
	public @Nullable String getMinute() {
		return minute;
	}

	/**
	 * Indicates whether hasMinute is satisfied.
	 */
	public boolean hasMinute() {
		return getMinute() != null;
	}

	/**
	 * Sets the minute.
	 *
	 * @return This expression for fluent configuration.
	 */
	public @Nonnull CronExpression setMinute(@Nullable String minute) {
		this.minute = minute;
		return this;
	}

	/**
	 * Sets the minute.
	 *
	 * @return This expression for fluent configuration.
	 */
	public @Nonnull CronExpression setMinute(@Nonnull Integer minute) {
		return setMinute(String.valueOf(minute));
	}

	/**
	 * Returns the hour.
	 */
	public @Nullable String getHour() {
		return hour;
	}

	/**
	 * Indicates whether hasHour is satisfied.
	 */
	public boolean hasHour() {
		return getHour() != null;
	}

	/**
	 * Sets the hour.
	 *
	 * @return This expression for fluent configuration.
	 */
	public @Nonnull CronExpression setHour(@Nullable String hour) {
		this.hour = hour;
		return this;
	}

	/**
	 * Sets the hour.
	 *
	 * @return This expression for fluent configuration.
	 */
	public @Nonnull CronExpression setHour(@Nonnull Integer minute) {
		return setHour(String.valueOf(minute));
	}

	/**
	 * Returns the dayNumber.
	 */
	public @Nullable String getDayNumber() {
		return dayNumber;
	}

	/**
	 * Indicates whether hasDayNumber is satisfied.
	 */
	public boolean hasDayNumber() {
		return getDayNumber() != null;
	}

	/**
	 * Sets the dayNumber.
	 *
	 * @return This expression for fluent configuration.
	 */
	public @Nonnull CronExpression setDayNumber(@Nullable String dayNumber) {
		this.dayNumber = dayNumber;
		return this;
	}

	/**
	 * Sets the dayNumber.
	 *
	 * @return This expression for fluent configuration.
	 */
	public @Nonnull CronExpression setDayNumber(@Nonnull Integer minute) {
		return setDayNumber(String.valueOf(minute));
	}

	/**
	 * Returns the month.
	 */
	public @Nullable String getMonth() {
		return month;
	}

	/**
	 * Indicates whether hasMonth is satisfied.
	 */
	public boolean hasMonth() {
		return getMonth() != null;
	}

	/**
	 * Sets the month.
	 *
	 * @return This expression for fluent configuration.
	 */
	public @Nonnull CronExpression setMonth(@Nullable String month) {
		this.month = month;
		return this;
	}

	/**
	 * Sets the month.
	 *
	 * @return This expression for fluent configuration.
	 */
	public @Nonnull CronExpression setMonth(@Nonnull Integer minute) {
		return setMonth(String.valueOf(minute));
	}

	/**
	 * Returns the dayOfWeek.
	 */
	public @Nullable String getDayOfWeek() {
		return dayOfWeek;
	}

	/**
	 * Indicates whether hasDayOfWeek is satisfied.
	 */
	public boolean hasDayOfWeek() {
		return getDayOfWeek() != null;
	}

	/**
	 * Sets the dayOfWeek.
	 *
	 * @return This expression for fluent configuration.
	 */
	public @Nonnull CronExpression setDayOfWeek(@Nullable String dayOfWeek) {
		this.dayOfWeek = dayOfWeek;
		return this;
	}

	/**
	 * Sets the dayOfWeek.
	 *
	 * @return This expression for fluent configuration.
	 */
	public @Nonnull CronExpression setDayOfWeek(@Nonnull Integer minute) {
		return setDayOfWeek(String.valueOf(minute));
	}

	/**
	 * Indicates whether hasNothing is satisfied.
	 */
	public boolean hasNothing() {
		return !hasMinute() && !hasHour() && !hasDayNumber() && !hasMonth() && !hasDayOfWeek();
	}

	/**
	 * Converts this expression into a friendly phrase for common schedules.
	 *
	 * <p>Returns canned labels for well-known schedules (for example {@code daily} and
	 * {@code hourly}) and otherwise constructs a best-effort descriptive sentence.
	 *
	 * @return A natural-language description, or {@code null} when no fields are set.
	 */
	@SuppressWarnings({"boxing", "java:S3776"}) // Complexity OK
	public @Nullable String toNaturalLanguage() {
		if (!hasNothing()) {
			// check if it's one of our static expressions
			String expression = toString();
			if (expression.equals("0 0 0 1 1 *"))
				return "yearly";
			if (expression.equals("0 0 0 1 * *"))
				return "monthly";
			if (expression.equals("0 0 0 * * 0"))
				return "weekly";
			if (expression.equals("0 0 12 * * ?") || expression.equals("0 0 12 * * *"))
				return "midday";
			if (expression.equals("* * * * * ?") || expression.equals("* * * * * *"))
				return "every second";
			if (expression.equals("0 * * * * ?") || expression.equals("0 * * * * *"))
				return "every minute";
			if (expression.equals("0 0 * * * ?") || expression.equals("0 0 * * * *"))
				return "hourly";
			if (expression.equals("0 0 0 * * *"))
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
					SimpleDateFormat sdf = CORE.getDateFormat("MMMM");
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

	/**
	 * Returns the string representation of this instance.
	 */
	@Override
	@SuppressWarnings("boxing")
	public @Nonnull String toString() {
		return String.format("%s %s %s %s %s %s",
				hasSecond() ? getSecond() : 0,
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
	private static @Nonnull String getDayNumberSuffix(int day) {
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
