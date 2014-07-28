package org.skyve.domain.types;

import java.text.ParseException;
import java.util.Date;

import org.skyve.wildcat.util.TimeUtil;

/**
 * Date and time (usually without seconds).
 */
public class DateTime extends Date {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -3775522231628914522L;

	/**
	 * 
	 */
	public DateTime() {
		super();
		TimeUtil.clearMillisecondComponent(this);
	}

	/**
	 * 
	 * @param date
	 */
	public DateTime(long date) {
		super(date);
		TimeUtil.clearMillisecondComponent(this);
	}
	
	/**
	 * 
	 * @param date
	 */
	public DateTime(Date date) {
		this(date.getTime());
	}
	
	/**
	 * 
	 * @param serializedForm
	 * @throws ParseException
	 */
	public DateTime(String serializedForm) throws ParseException {
		this(TimeUtil.parseISODate(serializedForm));
	}

	/**
	 * 
	 */
	@Override
	public String toString() {
		return TimeUtil.formatISODate(this, true);
	}
}
