package org.skyve.wildcat.domain.messages;

import java.util.List;

import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.ErrorException;
import org.skyve.domain.messages.ErrorMessage;
import org.skyve.domain.messages.ValidationMessage;

public class ReferentialConstraintViolationException extends DomainException implements ErrorException {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 4458169944531617791L;

	private ValidationMessage validationMessageDelegate;

	public ReferentialConstraintViolationException(String message) {
		super(message);
		validationMessageDelegate = new ValidationMessage(message);
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
