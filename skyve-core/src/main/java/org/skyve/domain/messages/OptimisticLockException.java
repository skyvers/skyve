package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;

import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

/**
 * Thrown when the persistence layer detects a concurrent modification of a
 * {@link org.skyve.domain.PersistentBean}.
 *
 * <p>Skyve uses optimistic locking: each save records the user and timestamp in the
 * {@code bizLock} column. On a subsequent save, if the stored lock differs from the
 * lock the current user loaded, the layer throws this exception to prevent the earlier
 * change from being silently overwritten.
 *
 * <p>The exception message is localised and includes the username and timestamp of the
 * conflicting save. The {@link OperationType} distinguishes between an update collision
 * and a delete-then-update collision.
 *
 * @see org.skyve.domain.types.OptimisticLock
 * @see org.skyve.domain.PersistentBean#getBizLock()
 */
public class OptimisticLockException extends DomainException implements MessageException {
	private static final long serialVersionUID = 9168437033648462795L;

	private static final String UPDATE_MESSAGE_KEY = "exception.optimisticLock.update";
	private static final String DELETE_MESSAGE_KEY = "exception.optimisticLock.delete";
	
	/**
	 * Distinguishes between an update-on-updated-record collision and an
	 * update-on-deleted-record collision.
	 */
	public enum OperationType {
		update, delete;
	}

	private List<Message> messages = null;
	
	/**
	 * Creates a new OptimisticLockException instance.
	 * @param user the user
	 * @param operationType the operationType
	 * @param persistentLock the persistentLock
	 */
	public OptimisticLockException(User user, OperationType operationType, OptimisticLock persistentLock) {
		super("Optimistic Lock failed - updated by user " + 
				persistentLock.getUsername() + 
				" at " + 
				persistentLock.getTimestamp(), false);

		String timeStampDisplay = null;
		try {
			timeStampDisplay = user.getCustomer().getDefaultTimestampConverter().toDisplayValue(new Timestamp(persistentLock.getTimestamp().getTime()));
		}
		catch (@SuppressWarnings("unused") Exception e) {
			timeStampDisplay = persistentLock.getTimestamp().toString();
		}

		if (operationType == OperationType.update) {
			String message = Util.nullSafeI18n(UPDATE_MESSAGE_KEY, persistentLock.getUsername(), timeStampDisplay);
			messages = Collections.singletonList(new Message(message));
		}
		else if (operationType == OperationType.delete) {
			String message = Util.nullSafeI18n(DELETE_MESSAGE_KEY, persistentLock.getUsername(), timeStampDisplay);
			messages = Collections.singletonList(new Message(message));
		}
	}

	/**
	 * Returns the messages.
	 * @return the result
	 */
	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
