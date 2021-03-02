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
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 3895082658495913979L;

	/**
	 * 
	 */
	public DateOnly() {
		super();
		TimeUtil.clearTimeComponent(this);
	}

	/**
	 * 
	 * @param date
	 */
	public DateOnly(Date date) {
		this(date.getTime());
	}
	
	/**
	 * 
	 * @param date
	 */
	public DateOnly(long date) {
		super(date);
		TimeUtil.clearTimeComponent(this);
	}

	/*
	 * 
	 */
	public DateOnly(String serializedForm) throws ParseException {
		this(CORE.getSerializableDateFormat().parse(serializedForm).getTime());
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

	public DateOnly setLocalDate(LocalDate date) {
		setTime(date.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
		return this; // for EL or method chaining
	}

	public DateOnly setLocalDateTime(LocalDateTime date) {
		setTime(date.toLocalDate().atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
		return this; // for EL or method chaining
	}
}
