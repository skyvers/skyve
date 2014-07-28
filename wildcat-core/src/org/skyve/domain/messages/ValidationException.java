package org.skyve.domain.messages;

import java.util.List;

/**
 * 
 */
public class ValidationException extends DomainException implements ErrorException {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -2033700648810378214L;

	protected ValidationMessage message;

	/**
	 * 
	 * @param message
	 */
	public ValidationException(ValidationMessage message) {
		super(message.getErrorMessage());
		this.message = message;
	}

	/**
	 * 
	 */
	@Override
	public void addBinding(String binding) {
		message.addBinding(binding);
	}

	/**
	 * 
	 */
	@Override
	public Iterable<String> getBindings() {
		return message.getBindings();
	}

	/**
	 * 
	 */
	@Override
	public String getErrorMessage() {
		return message.getErrorMessage();
	}

	/**
	 * 
	 */
	@Override
	public List<ErrorMessage> getSubordinates() {
		return message.getSubordinates();
	}

	/**
	 * 
	 */
	@Override
	public void setBindingPrefix(String bindingPrefixWithDot) {
		message.setBindingPrefix(bindingPrefixWithDot);
	}

	/**
	 * 
	 */
	@Override
	public String toString() {
		return super.toString() + "\n" + message.toString();
	}

	/**
	 * 
	 */
	@Override
	public ValidationMessage getDelegate() {
		return message;
	}
}
