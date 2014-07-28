package org.skyve.domain.messages;

import java.util.List;

/**
 * 
 */
public class SessionEndedException extends DomainException implements ErrorException {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2247724782906480914L;

	private static final String MESSAGE = "Your session has ended - press F5 key or the Refresh/Reload browser button.";
	private ValidationMessage validationMessageDelegate;

	/**
	 * 
	 * @param constraintName
	 * @param message
	 */
	public SessionEndedException() {
		super(MESSAGE);
		validationMessageDelegate = new ValidationMessage(MESSAGE);
	}

	/**
	 * 
	 */
	@Override
	public void addBinding(String binding) {
		validationMessageDelegate.addBinding(binding);
	}

	/**
	 * 
	 */
	@Override
	public Iterable<String> getBindings() {
		return validationMessageDelegate.getBindings();
	}

	/**
	 * 
	 */
	@Override
	public String getErrorMessage() {
		return validationMessageDelegate.getErrorMessage();
	}

	/**
	 * 
	 */
	@Override
	public List<ErrorMessage> getSubordinates() {
		return validationMessageDelegate.getSubordinates();
	}

	/**
	 * 
	 */
	@Override
	public void setBindingPrefix(String bindingPrefixWithDot) {
		validationMessageDelegate.setBindingPrefix(bindingPrefixWithDot);
	}

	/**
	 * 
	 */
	@Override
	public ValidationMessage getDelegate() {
		return validationMessageDelegate;
	}
}
