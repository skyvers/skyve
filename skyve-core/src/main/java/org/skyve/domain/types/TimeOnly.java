package org.skyve.domain.types;

import java.text.ParseException;
import java.util.Date;

import org.skyve.CORE;
import org.skyve.impl.util.TimeUtil;

/**
 * Time (usually without seconds).
 */
public class TimeOnly extends Date {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -855625417592920463L;

	/**
	 * 
	 */
	public TimeOnly() {
		super();
		TimeUtil.clearMillisecondComponent(this);
	}

	/**
	 * 
	 * @param date
	 */
	public TimeOnly(long date) {
		super(date);
		TimeUtil.clearDateComponent(this);
		TimeUtil.clearMillisecondComponent(this);
	}

	/**
	 * 
	 * @param date
	 */
	public TimeOnly(Date date) {
		this(date.getTime());
	}
	
	/**
	 * 
	 * @param hours24
	 * @param minutes
	 * @param seconds
	 */
	public TimeOnly(int hours24, int minutes, int seconds) {
		super(0);
		TimeUtil.setTime(this, hours24, minutes, seconds);
	}

	/**
	 * 
	 * @param serializedForm
	 * @throws ParseException
	 */
	public TimeOnly(String serializedForm) throws ParseException {
		this(CORE.getSerializableTimeFormat().parse(serializedForm).getTime());
	}

	/**
	 * 
	 */
	@Override
	public String toString() {
		return CORE.getSerializableTimeFormat().format(this);
	}
}
