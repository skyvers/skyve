package org.skyve.domain.messages;

import java.util.List;

import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.customer.Customer;

/**
 * 
 */
public class OptimisticLockException extends DomainException implements ErrorException {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 9168437033648462795L;

	private ValidationMessage validationMessageDelegate;

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
			validationMessageDelegate = new ValidationMessage("Failed to update this information as it was updated by user " +
																persistentLock.getUsername() + 
																" at " + 
																timeStampDisplay +
																" after you looked at it.  The information is now refreshed.  Please re-apply your changes.");
		}
		else if (operationType == OperationType.delete) {
			validationMessageDelegate = new ValidationMessage("Failed to delete this information as it was updated by user " +
																persistentLock.getUsername() + 
																" at " + 
																timeStampDisplay +
																" after you looked at it.  The information is now refreshed.  Please review and delete if still necessary.");
		}
	}

	@Override
	public String getMessage() {
		return getErrorMessage();
	}

	@Override
	public String getLocalizedMessage() {
		return getMessage();
	}

	@Override
	public String toString() {
		return getMessage();
	}

	@Override
	public void addBinding(String binding) {
		validationMessageDelegate.addBinding(binding);
	}

	@Override
	public Iterable<String> getBindings() {
		return validationMessageDelegate.getBindings();
	}

	@Override
	public String getErrorMessage() {
		return validationMessageDelegate.getErrorMessage();
	}

	@Override
	public List<ErrorMessage> getSubordinates() {
		return validationMessageDelegate.getSubordinates();
	}

	@Override
	public void setBindingPrefix(String bindingPrefixWithDot) {
		validationMessageDelegate.setBindingPrefix(bindingPrefixWithDot);
	}

	@Override
	public ValidationMessage getDelegate() {
		return validationMessageDelegate;
	}
}
