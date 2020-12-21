package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;

import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

/**
 * 
 */
public class OptimisticLockException extends DomainException implements MessageException {
	private static final long serialVersionUID = 9168437033648462795L;

	private static final String UPDATE_MESSAGE_KEY = "exception.optimisticLock.update";
	private static final String DELETE_MESSAGE_KEY = "exception.optimisticLock.delete";
	
	public enum OperationType {
		update, delete;
	}

	private List<Message> messages = null;
	
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
			messages = Collections.singletonList(new Message(Util.i18n(UPDATE_MESSAGE_KEY, user.getLocale(), persistentLock.getUsername(), timeStampDisplay)));
		}
		else if (operationType == OperationType.delete) {
			messages = Collections.singletonList(new Message(Util.i18n(DELETE_MESSAGE_KEY, user.getLocale(), persistentLock.getUsername(), timeStampDisplay)));
		}
	}

	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
