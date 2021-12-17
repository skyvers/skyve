package org.skyve.domain.types;

import java.text.ParseException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;

import org.skyve.impl.util.TimeUtil;

/**
 * Date and time (without seconds).
 */
public class DateTime extends Date {
	private static final long serialVersionUID = -3775522231628914522L;

	public DateTime() {
		super();
		TimeUtil.clearMillisecondComponent(this);
	}

	public DateTime(long date) {
		super(date);
		TimeUtil.clearMillisecondComponent(this);
	}
	
	public DateTime(Date date) {
		this(date.getTime());
	}
	
	public DateTime(String serializedForm) throws ParseException {
		this(TimeUtil.parseISODate(serializedForm));
	}

	public DateTime(LocalDate date) {
		this(date.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
	}
	
	public DateTime(LocalDateTime date) {
		this(date.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
	}
	
	@Override
	public String toString() {
		return TimeUtil.formatISODate(this, true);
	}
	
	public LocalDate toLocalDate() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
	}

	public LocalDateTime toLocalDateTime() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
	}

	public LocalTime toLocalTime() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalTime();
	}
}
