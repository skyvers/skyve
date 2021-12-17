package org.skyve.domain.types;

import java.text.ParseException;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;

import org.skyve.CORE;
import org.skyve.impl.util.TimeUtil;

/**
 * Time (without seconds).
 */
public class TimeOnly extends Date {
	private static final long serialVersionUID = -855625417592920463L;

	public TimeOnly() {
		super();
		TimeUtil.clearDateComponent(this);
		TimeUtil.clearMillisecondComponent(this);
	}

	public TimeOnly(long date) {
		super(date);
		TimeUtil.clearDateComponent(this);
		TimeUtil.clearMillisecondComponent(this);
	}

	public TimeOnly(Date date) {
		this(date.getTime());
	}
	
	public TimeOnly(int hours24, int minutes, int seconds) {
		super(0);
		TimeUtil.setTime(this, hours24, minutes, seconds);
	}

	public TimeOnly(String serializedForm) throws ParseException {
		this(CORE.getSerializableTimeFormat().parse(serializedForm).getTime());
	}

	public TimeOnly(LocalTime time) {
		this(time.getHour(), time.getMinute(), time.getSecond());
	}

	public TimeOnly(LocalDateTime date) {
		this(date.toLocalTime());
	}

	@Override
	public String toString() {
		return CORE.getSerializableTimeFormat().format(this);
	}
	
	public LocalTime toLocalTime() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalTime();
	}
}
