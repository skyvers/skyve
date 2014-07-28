package org.skyve.domain.types;

import java.text.ParseException;
import java.util.Date;

import org.skyve.wildcat.util.TimeUtil;

/**
 * Date and time (usually with seconds)
 */
public class Timestamp extends Date {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 4574696935450819323L;

	/**
	 * 
	 */
	public Timestamp() {
		super();
		TimeUtil.clearMillisecondComponent(this);
	}

	/**
	 * 
	 * @param date
	 */
	public Timestamp(long date) {
		super(date);
		TimeUtil.clearMillisecondComponent(this);
	}

	/**
	 * 
	 * @param date
	 */
	public Timestamp(Date date) {
		this(date.getTime());
	}
	
	/**
	 * 
	 * @param serializedForm
	 * @throws ParseException
	 */
	public Timestamp(String serializedForm) throws ParseException {
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
