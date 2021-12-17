package org.skyve.domain.types;

import java.text.ParseException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;

import org.skyve.CORE;
import org.skyve.impl.util.TimeUtil;

/**
 * Date with no time - 12 midnight.
 */
public class DateOnly extends Date {
	private static final long serialVersionUID = 3895082658495913979L;

	public DateOnly() {
		super();
		TimeUtil.clearTimeComponent(this);
	}

	public DateOnly(Date date) {
		this(date.getTime());
	}
	
	public DateOnly(long date) {
		super(date);
		TimeUtil.clearTimeComponent(this);
	}

	public DateOnly(String serializedForm) throws ParseException {
		this(CORE.getSerializableDateFormat().parse(serializedForm).getTime());
	}

	public DateOnly(LocalDate date) {
		this(date.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
	}
	
	public DateOnly(LocalDateTime date) {
		this(date.toLocalDate().atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
	}
	
	@Override
	public String toString() {
		return CORE.getSerializableDateFormat().format(this);
	}
	
	public LocalDate toLocalDate() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
	}

	public LocalDateTime toLocalDateTime() {
		return toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
	}
}
