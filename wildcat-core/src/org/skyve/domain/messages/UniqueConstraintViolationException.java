package org.skyve.domain.messages;

import java.util.List;

/**
 * 
 */
public class UniqueConstraintViolationException extends DomainException implements ErrorException {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2245888585799230814L;

	private ValidationMessage validationMessageDelegate;

	private String constraintName;
	
	/**
	 * 
	 * @param constraintName
	 * @param message
	 */
	public UniqueConstraintViolationException(String constraintName, String message) {
		super(message);
		validationMessageDelegate = new ValidationMessage(message);
		this.constraintName = constraintName;
	}

	/**
	 * 
	 * @return
	 */
	public String getConstraintName() {
		return constraintName;
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
