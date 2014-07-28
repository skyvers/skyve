package org.skyve.domain.types;

import java.io.Serializable;
import java.text.ParseException;
import java.util.Date;

import org.skyve.domain.messages.DomainException;
import org.skyve.wildcat.util.ThreadSafeFactory;

/**
 * 
 */
public class OptimisticLock implements Serializable {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -188896815713122L;

	private static final String LOCK_TIMESTAMP_FORMAT = "yyyyMMddHHmmssSSS";

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
	 * @throws DomainException
	 */
	public OptimisticLock(String lockString) throws DomainException {
		if ((lockString == null) || (lockString.length() < 18)) {
			throw new DomainException("bizLock " + lockString + " is invalid");
		}
		
		this.lockUsername = lockString.substring(17);
		try {
			this.lockTimestamp = ThreadSafeFactory.getDateFormat(LOCK_TIMESTAMP_FORMAT).parse(lockString.substring(0, 17));
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
		return ThreadSafeFactory.getDateFormat(LOCK_TIMESTAMP_FORMAT).format(lockTimestamp) + lockUsername;
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
