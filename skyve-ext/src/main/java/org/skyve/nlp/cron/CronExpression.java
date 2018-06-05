package org.skyve.nlp.cron;

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
}
