package org.skyve.domain.types;

import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;

/**
 * Records the user and UTC timestamp of the last successful save of a
 * {@link org.skyve.domain.PersistentBean}.
 *
 * <p>The persistence layer stores this value in the {@code bizLock} column as a
 * compact string: a 17-character UTC timestamp in {@code yyyyMMddHHmmssSSS} format
 * followed immediately by the username (e.g. {@code 20240115143045123admin}).
 * When a concurrent save attempt is detected, the layer throws
 * {@link org.skyve.domain.messages.OptimisticLockException}.
 *
 * <p>The string constructor parses this format; the two-argument constructor accepts
 * explicit username and timestamp values.
 *
 * <p>Equality is based on both username and timestamp being equal.
 *
 * <p>Threading: instances are mutable (setters are public) but are not shared across
 * threads in normal use.
 *
 * @see org.skyve.domain.PersistentBean#getBizLock()
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
	 * Constructs an {@code OptimisticLock} from explicit components.
	 *
	 * @param username  the name of the user who performed the save; must not be {@code null}
	 * @param timestamp the UTC timestamp of the save; must not be {@code null}
	 */
	public OptimisticLock(String username, Date timestamp) {
		this.lockUsername = username;
		this.lockTimestamp = timestamp;
	}

	/**
	 * Constructs an {@code OptimisticLock} by parsing the compact string representation
	 * stored in the {@code bizLock} column (17 timestamp characters followed by the username).
	 *
	 * @param lockString the stored lock string; must be at least 18 characters long
	 * @throws org.skyve.domain.messages.DomainException if {@code lockString} is {@code null},
	 *         shorter than 18 characters, or contains an unparseable timestamp
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
	 * Returns this lock as the compact string stored in the {@code bizLock} column
	 * (17-character UTC timestamp followed by username).
	 *
	 * @return the serialized lock string; never {@code null}
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
		if (object instanceof OptimisticLock otherLock) {
			return (lockUsername.equals(otherLock.lockUsername) && lockTimestamp.equals(otherLock.lockTimestamp));
		}

		return false;
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
	 * Returns the UTC timestamp at which the save was recorded.
	 *
	 * @return the lock timestamp; never {@code null} for a properly constructed lock
	 */
	public Date getTimestamp() {
		return lockTimestamp;
	}

	/**
	 * Sets the timestamp of this lock. Normally only called by the persistence layer.
	 *
	 * @param timestamp the new timestamp; must not be {@code null}
	 */
	public void setTimestamp(Date timestamp) {
		this.lockTimestamp = timestamp;
	}

	/**
	 * Returns the name of the user who last saved the bean.
	 *
	 * @return the username; never {@code null} for a properly constructed lock
	 */
	public String getUsername() {
		return lockUsername;
	}

	/**
	 * Sets the username of this lock. Normally only called by the persistence layer.
	 *
	 * @param username the new username; must not be {@code null}
	 */
	public void setUsername(String username) {
		this.lockUsername = username;
	}
}
