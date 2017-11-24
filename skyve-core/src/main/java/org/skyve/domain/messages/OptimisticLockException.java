package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.customer.Customer;

/**
 * 
 */
public class OptimisticLockException extends DomainException implements MessageException {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 9168437033648462795L;

	/**
	 * 
	 */
	public enum OperationType {
		/**
		 * 
		 */
		update,
		
		/**
		 * 
		 */
		delete;
	}

	private List<Message> messages = new ArrayList<>(1);
	
	public OptimisticLockException(Customer customer, OperationType operationType, OptimisticLock persistentLock) {
		super("Optimistic Lock failed - updated by user " + 
				persistentLock.getUsername() + 
				" at " + 
				persistentLock.getTimestamp());

		String timeStampDisplay = null;
		try {
			timeStampDisplay = customer.getDefaultTimestampConverter().toDisplayValue(new Timestamp(persistentLock.getTimestamp().getTime()));
		}
		catch (Exception e) {
			timeStampDisplay = persistentLock.getTimestamp().toString();
		}

		if (operationType == OperationType.update) {
			messages.add(new Message("Failed to update this information as it was updated by user " +
										persistentLock.getUsername() + 
										" at " + 
										timeStampDisplay +
										" after you looked at it.  Please re-apply your changes."));
		}
		else if (operationType == OperationType.delete) {
			messages.add(new Message("Failed to delete this information as it was updated by user " +
										persistentLock.getUsername() + 
										" at " + 
										timeStampDisplay +
										" after you looked at it.  Please review and delete if still necessary."));
		}
	}

	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
