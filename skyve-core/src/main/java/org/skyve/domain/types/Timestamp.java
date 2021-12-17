package org.skyve.domain.types;

import java.text.ParseException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.Date;

import org.skyve.impl.util.TimeUtil;

/**
 * Date and time (with seconds)
 */
public class Timestamp extends Date {
	private static final long serialVersionUID = 4574696935450819323L;

	public Timestamp() {
		super();
		TimeUtil.clearMillisecondComponent(this);
	}

	public Timestamp(long date) {
		super(date);
		TimeUtil.clearMillisecondComponent(this);
	}

	public Timestamp(Date date) {
		this(date.getTime());
	}
	
	public Timestamp(String serializedForm) throws ParseException {
		this(TimeUtil.parseISODate(serializedForm));
	}

	public Timestamp(LocalDate date) {
		this(date.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
	}
	
	public Timestamp(LocalDateTime date) {
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
