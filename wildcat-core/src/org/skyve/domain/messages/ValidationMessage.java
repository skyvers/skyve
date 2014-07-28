package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

/**
 * 
 */
public class ValidationMessage implements ErrorMessage {
	private List<String> bindings = new ArrayList<>();

	private String errorMessage;

	private List<ErrorMessage> subordinates = new ArrayList<>();

	/**
	 * 
	 * @param errorMessage
	 */
	public ValidationMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	/**
	 * Convenience constructor for 1 binding.
	 * 
	 * @param binding
	 * @param errorMessage
	 */
	public ValidationMessage(String binding, String errorMessage) {
		this.errorMessage = errorMessage;
		bindings.add(binding);
	}

	/**
	 * 
	 * @param bindings
	 * @param errorMessage
	 */
	public ValidationMessage(String[] bindings, String errorMessage) {
		this.errorMessage = errorMessage;

		for (String binding : bindings) {
			this.bindings.add(binding);
		}
	}

	/**
	 * 
	 */
	@Override
	public void setBindingPrefix(String bindingPrefixWithDot) {
		for (int i = 0, l = bindings.size(); i < l; i++) {
			String binding = bindings.remove(i);
			bindings.add(i, bindingPrefixWithDot + binding);
		}

		for (ErrorMessage subordinate : subordinates) {
			subordinate.setBindingPrefix(bindingPrefixWithDot);
		}
	}

	/**
	 * 
	 */
	@Override
	public void addBinding(String binding) {
		bindings.add(binding);
	}

	/**
	 * 
	 */
	@Override
	public Iterable<String> getBindings() {
		return bindings;
	}

	/**
	 * 
	 */
	@Override
	public String getErrorMessage() {
		return errorMessage;
	}

	/**
	 * 
	 */
	@Override
	public List<ErrorMessage> getSubordinates() {
		return subordinates;
	}

	/**
	 * 
	 */
	@Override
	public String toString() {
		StringBuilder result = new StringBuilder(64);
		result.append("Bindings = ");
		for (String binding : bindings) {
			result.append(binding).append(" : ");
		}
		result.append('\n');
		result.append("Error message = ").append(errorMessage).append('\n');

		for (ErrorMessage subordinate : subordinates) {
			result.append(subordinate.toString());
		}

		return result.toString();
	}
}
