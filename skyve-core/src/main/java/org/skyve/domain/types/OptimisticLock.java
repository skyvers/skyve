package org.skyve.domain.types;

import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;

/**
 * 
 */
public class OptimisticLock implements Serializable {
	private static final long serialVersionUID = -188896815713122L;

	private static final String LOCK_TIMESTAMP_FORMAT = "yyyyMMddHHmmssSSS";
	private static final TimeZone LOCK_TIMEZONE = TimeZone.getTimeZone("UTC");
	
	private String lockUsername;

	private Date lockTimestamp;

	/**
	 * Constructor for persistence
	 */
	@SuppressWarnings("unused")
	private OptimisticLock() {
		// do nothing
	}

	/**
	 * 
	 * @param username
	 * @param timestamp
	 */
	public OptimisticLock(String username, Date timestamp) {
		this.lockUsername = username;
		this.lockTimestamp = timestamp;
	}

	/**
	 * 
	 * @param lockString
	 */
	public OptimisticLock(String lockString) {
		if ((lockString == null) || (lockString.length() < 18)) {
			throw new DomainException("bizLock " + lockString + " is invalid");
		}
		
		this.lockUsername = lockString.substring(17);
		try {
			SimpleDateFormat format = CORE.getDateFormat(LOCK_TIMESTAMP_FORMAT);
			format.setTimeZone(LOCK_TIMEZONE);
			this.lockTimestamp = format.parse(lockString.substring(0, 17));
		}
		catch (ParseException e) {
			throw new DomainException("Exception parsing " + lockString, e);
		}
	}

	/**
	 * 
	 */
	@Override
	public String toString() {
		SimpleDateFormat format = CORE.getDateFormat(LOCK_TIMESTAMP_FORMAT);
		format.setTimeZone(LOCK_TIMEZONE);
		return format.format(lockTimestamp) + lockUsername;
	}

	/**
	 * 
	 */
	@Override
	public boolean equals(Object object) {
		boolean equal = (object instanceof OptimisticLock);
		if (equal) {
			OptimisticLock otherLock = (OptimisticLock) object;
			equal = (lockUsername.equals(otherLock.lockUsername) && lockTimestamp.equals(otherLock.lockTimestamp));
		}

		return equal;
	}

	/**
	 * 
	 */
	@Override
	public int hashCode() {
		int result = 17;
		result = 37 * result + lockUsername.hashCode();
		result = 37 * result + lockTimestamp.hashCode();

		return result;
	}

	/**
	 * 
	 * @return
	 */
	public Date getTimestamp() {
		return lockTimestamp;
	}

	/**
	 * 
	 * @param timestamp
	 */
	public void setTimestamp(Date timestamp) {
		this.lockTimestamp = timestamp;
	}

	/**
	 * 
	 * @return
	 */
	public String getUsername() {
		return lockUsername;
	}

	/**
	 * 
	 * @param username
	 */
	public void setUsername(String username) {
		this.lockUsername = username;
	}
}
